use std::{collections::HashMap, path::PathBuf, sync::Arc};

use alacritty_terminal::{
    event::{Event as ChildEvent, EventListener, WindowSize},
    event_loop::{EventLoop, EventLoopSender, Msg},
    sync::FairMutex,
    term::{
        cell::Flags,
        color::{Colors, COUNT as COLOR_COUNT},
        test::TermSize,
        Config,
    },
    tty::Options,
    vte::ansi::{Color as AnsiColor, CursorShape, NamedColor, Rgb},
    Term,
};
use bevy::{
    asset::embedded_asset,
    input::keyboard::Key,
    prelude::*,
    render::{
        render_asset::RenderAssetUsages,
        render_resource::{AsBindGroup, Extent3d, ShaderRef, TextureDimension, TextureFormat},
    },
};
use bevy_msdf::{MsdfAtlas, MsdfDraw, MsdfGlyph, MsdfPlugin};
use bevy_vector_shapes::{
    painter::{ShapeConfig, ShapePainter},
    shapes::RectPainter,
    ShapePlugin,
};
use flume::{Receiver, Sender};
use owned_ttf_parser::{AsFaceRef, GlyphId};

pub use bevy_msdf;

pub struct TerminalPlugin;

impl Plugin for TerminalPlugin {
    fn build(&self, app: &mut App) {
        if !app.is_plugin_added::<MsdfPlugin>() {
            app.add_plugins(MsdfPlugin);
        }

        if !app.is_plugin_added::<ShapePlugin>() {
            app.add_plugins(ShapePlugin {
                base_config: ShapeConfig::default_3d(),
                exclude_2d: false,
            });
        }

        app.add_plugins(MaterialPlugin::<BgMaterial>::default())
            .add_systems(
                PreUpdate,
                (
                    process_events,
                    render.after(process_events),
                    remove_children.after(render),
                ),
            );

        embedded_asset!(app, "bg.wgsl");
    }
}

#[derive(Component)]
struct TerminalChild;

fn process_events(mut commands: Commands, query: Query<(Entity, &Terminal)>) {
    for (e, term) in query.iter() {
        while let Ok(ev) = term.event_rx.try_recv() {
            match ev {
                ChildEvent::ColorRequest(idx, fmt) => {
                    term.send_input(&fmt(term.colors[idx].unwrap_or(Rgb {
                        r: 0xff,
                        g: 0x00,
                        b: 0xff,
                    })));
                }
                ChildEvent::PtyWrite(text) => {
                    term.send_input(&text);
                }
                ChildEvent::Exit => {
                    commands.entity(e).despawn_recursive();
                }

                // TODO: add title as field
                ChildEvent::Title(_) => {}
                ChildEvent::ResetTitle => {}

                // TODO: only redraw glyphs and bg after receiving this
                ChildEvent::Wakeup => {}

                // TODO: handle bell?
                ChildEvent::Bell => {}

                // TODO: idk these might be good for something
                ChildEvent::MouseCursorDirty
                | ChildEvent::ClipboardStore(_, _)
                | ChildEvent::ClipboardLoad(_, _)
                | ChildEvent::TextAreaSizeRequest(_)
                | ChildEvent::CursorBlinkingChange
                | ChildEvent::ChildExit(_) => {}
            }
        }
    }
}

fn remove_children(
    mut commands: Commands,
    mut removed: RemovedComponents<Parent>,
    query: Query<(), With<TerminalChild>>,
) {
    for e in removed.read() {
        if query.contains(e) {
            commands.entity(e).despawn_recursive();
        }
    }
}

fn render(
    mut commands: Commands,
    mut painter: ShapePainter,
    mut images: ResMut<Assets<Image>>,
    atlases: Res<Assets<MsdfAtlas>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<BgMaterial>>,
    mut query: Query<(
        Entity,
        &mut Terminal,
        &GlobalTransform,
        Option<&InheritedVisibility>,
    )>,
) {
    for (e, mut terminal, transform, visibility) in query.iter_mut() {
        // retrieve handles to the terminal's atlases
        let try_atlases = terminal.fonts.as_ref().map(|font| atlases.get(font));

        // ensure all atlases are loaded
        let Some(atlases) = try_atlases.all() else {
            continue;
        };

        // start by computing the glyph metrics of the terminal
        let metrics = terminal
            .metrics
            .get_or_insert_with(|| TerminalMetrics::new(atlases.clone()))
            .clone();

        // recalculate grid size
        let available = terminal.space.size - terminal.space.padding * 2.0;
        let grid_size = available / metrics.cell_size / terminal.space.units_per_em;
        let grid_size = grid_size.floor().as_uvec2();

        // resize terminal if needed; defer new TermSize until we've locked term
        let mut resize = None;
        if grid_size != terminal.grid_size {
            // update the terminal's cached grid size
            terminal.grid_size = grid_size;

            // defer resizing the term field until we have a lock
            resize = Some(TermSize::new(grid_size.x as usize, grid_size.y as usize));

            // update the child that a resize has occurred
            let _ = terminal.event_tx.send(Msg::Resize(WindowSize {
                num_lines: grid_size.y as u16,
                num_cols: grid_size.x as u16,
                cell_width: 1,
                cell_height: 1,
            }));
        }

        let bg_extent = Extent3d {
            width: terminal.grid_size.x + 2,
            height: terminal.grid_size.y + 2,
            depth_or_array_layers: 1,
        };

        let bg_data_len = (bg_extent.width * bg_extent.height) as usize * 4;
        let mut bg_data = vec![0u32; (bg_extent.width * bg_extent.height) as usize];

        let mut term = terminal.term.lock();

        // process deferred terminal resize
        if let Some(resize) = resize {
            term.resize(resize);
        }

        // after resizing if necessary, if this terminal is hidden, hide all
        // children and abort rendering
        if visibility.copied() == Some(InheritedVisibility::HIDDEN) {
            commands.entity(e).despawn_descendants();
            continue;
        }

        // begin rendering the terminal's content
        let content = term.renderable_content();

        // merge content into current terminal colors
        // we'll write this back into terminal once we have mutable access again
        let mut colors = terminal.colors;
        for index in 0..COLOR_COUNT {
            if let Some(color) = content.colors[index] {
                colors[index] = Some(color);
            }
        }

        // draw background panel
        let bg_color = AnsiColor::Named(NamedColor::Background);
        let bg_color = color_to_bevy(&colors, bg_color).with_a(terminal.space.opacity);
        painter.origin = Some(transform.transform_point(Vec3::Z * -0.1));
        painter.transform = transform.compute_transform();
        painter.corner_radii = terminal.space.corner_radii;
        painter.color = bg_color;
        painter.rect(terminal.space.size);

        // draw foreground objects in front of background
        painter.origin = Some(transform.transform_point(Vec3::Z * 0.1));

        // helper function to transform grid points to panel-space coords
        let grid_to_pos = |x: i32, y: i32| -> Vec2 {
            let mut pos = IVec2::new(x, y).as_vec2() - grid_size.as_vec2() / 2.0;
            pos.y = -pos.y;
            pos * metrics.cell_size * terminal.space.units_per_em
        };

        // helper function to draw a solid rectangle in terminal space
        let draw_rect = |painter: &mut ShapePainter, tl: Vec2, br: Vec2| {
            let center = (tl + br) / 2.0;
            let size = (tl - br).abs();
            let local = Transform::from_translation(center.extend(0.001));
            painter.transform = transform.compute_transform() * local;
            painter.rect(size);
        };

        let mut glyphs = FontSet::<Vec<MsdfGlyph>>::default();
        let glyph_offset = (grid_size.as_vec2() * metrics.cell_size) * -0.5;
        for cell in content.display_iter {
            // grab terminal coordinates of this cell
            let row = cell.point.line.0;
            let col = cell.point.column.0 as i32;

            // calculate color values
            let mut fg = color_to_bevy(&colors, cell.fg);
            let mut bg = color_to_bevy(&colors, cell.bg);

            // swap colors if needed
            if cell.flags.contains(Flags::INVERSE) {
                std::mem::swap(&mut fg, &mut bg);
            }

            // if background is not the transparent color, write to bg texture
            if bg != bg_color.with_a(1.0) {
                let bg_idx = (row as u32 + 1) * bg_extent.width + (col as u32 + 1);
                let bg_cell = &mut bg_data[bg_idx as usize];
                *bg_cell = bg.as_rgba_u32();
            }

            // look up the correct font atlas and metrics for this cell's style
            let style = FontStyle::from_cell_flags(cell.flags);
            let atlas = atlases.get(style);
            let font_metrics = metrics.fonts.get(style);

            // calc common line variables
            let tl = grid_to_pos(col, row);
            let br = grid_to_pos(col + 1, row + 1);
            let baseline = *metrics.baselines.get(style) * terminal.space.units_per_em;
            painter.color = fg;

            // helper function to make a horizontal line
            let make_line = |pos, width| -> (Vec2, Vec2) {
                let cy = tl.y + pos * terminal.space.units_per_em - baseline;
                let w = width * terminal.space.units_per_em;
                let tl = Vec2::new(tl.x, cy);
                let br = Vec2::new(br.x, cy + w);
                (tl, br)
            };

            // draw strikeout if needed
            if cell.flags.contains(Flags::STRIKEOUT) {
                let so_line = make_line(font_metrics.strikeout_pos, font_metrics.strikeout_width);
                draw_rect(&mut painter, so_line.0, so_line.1);
            }

            // draw underline if needed
            if cell.flags.contains(Flags::UNDERLINE) {
                let ul_line = make_line(font_metrics.underline_pos, font_metrics.underline_width);
                draw_rect(&mut painter, ul_line.0, ul_line.1);
            }

            // pre-emptively skip drawing space characters
            if cell.c == ' ' {
                continue;
            }

            // attempt to look up glyph
            let Some(GlyphId(index)) = atlas.face.as_face_ref().glyph_index(cell.c) else {
                continue;
            };

            // draw the glyph
            let glyphs = glyphs.get_mut(style);
            let xy = IVec2::new(col, grid_size.y as i32 - row);
            let mut pos = xy.as_vec2() * metrics.cell_size + glyph_offset;
            pos.y -= metrics.baselines.get(style);
            let color = fg;
            glyphs.push(MsdfGlyph { pos, color, index });
        }

        // begin drawing cursor
        let col = content.cursor.point.column.0 as i32;
        let row = content.cursor.point.line.0;
        let tl = grid_to_pos(col, row);
        let br = grid_to_pos(col + 1, row + 1);
        let cursor_width = 0.1 * terminal.space.units_per_em;
        painter.color = color_to_bevy(&colors, AnsiColor::Named(NamedColor::Foreground));
        painter.corner_radii = Vec4::ZERO;

        // draw the cursor
        match content.cursor.shape {
            CursorShape::Hidden => {}
            CursorShape::Block => {
                draw_rect(&mut painter, tl, br);
            }
            CursorShape::Underline => {
                let tl = Vec2::new(tl.x, br.y + cursor_width);
                draw_rect(&mut painter, tl, br);
            }
            CursorShape::Beam => {
                let br = Vec2::new(tl.x + cursor_width, br.y);
                draw_rect(&mut painter, tl, br);
            }
            CursorShape::HollowBlock => {
                painter.hollow = true;
                painter.thickness = cursor_width;
                draw_rect(&mut painter, tl, br);
            }
        }

        // drop term lock so that we can mutate terminal again
        drop(term);

        // write content's colors back into terminal
        terminal.colors = colors;

        // begin updating the background image
        // glyphs are in em-space, so we need to transform them into world space
        let bg_size = (grid_size + 2).as_vec2() * metrics.cell_size * terminal.space.units_per_em;
        let glyph_transform = Transform::from_scale(Vec3::splat(terminal.space.units_per_em));

        let mut new_children = vec![];

        let bg_handle = terminal.bg.get_or_insert_with(|| images.reserve_handle());
        let bg = images.get_or_insert_with(bg_handle.clone(), || {
            Image::new(
                bg_extent,
                TextureDimension::D2,
                vec![0xff; bg_data_len],
                TextureFormat::Rgba8UnormSrgb,
                RenderAssetUsages::all(),
            )
        });

        bg.data.resize(bg_data_len, 0xff);
        bg.data
            .copy_from_slice(bytemuck::cast_slice(bg_data.as_slice()));
        bg.texture_descriptor.size = bg_extent;

        new_children.push(
            commands
                .spawn((
                    MaterialMeshBundle {
                        mesh: meshes.add(
                            Rectangle::from_size(bg_size)
                                .mesh()
                                .translated_by(Vec3::Z * 0.05),
                        ),
                        material: materials.add(BgMaterial {
                            bg: bg_handle.clone(),
                        }),
                        transform: Transform::from_translation(Vec3::Z * -0.05),
                        ..default()
                    },
                    TerminalChild,
                ))
                .id(),
        );

        glyphs
            .zip(terminal.fonts.clone())
            .for_each(|(glyphs, atlas)| {
                if glyphs.is_empty() {
                    return;
                }

                new_children.push(
                    commands
                        .spawn((
                            MsdfDraw { atlas, glyphs },
                            glyph_transform,
                            GlobalTransform::default(),
                            TerminalChild,
                        ))
                        .id(),
                );
            });

        commands.entity(e).replace_children(&new_children);
    }
}

/// The geometry of a terminal's space.
#[derive(Clone, Debug, Reflect)]
pub struct TerminalSpace {
    /// The total size of the terminal.
    pub size: Vec2,

    /// A scaling factor of font em units to world-space units.
    pub units_per_em: f32,

    /// The corner radii of the terminal.
    pub corner_radii: Vec4,

    /// The minimum padding between the terminal's size and inner grid.
    pub padding: Vec2,

    /// The terminal's background opacity.
    pub opacity: f32,
}

impl Default for TerminalSpace {
    fn default() -> Self {
        Self {
            size: Vec2::new(1.2, 0.8),
            units_per_em: 0.015,
            corner_radii: Vec4::splat(0.01),
            padding: Vec2::splat(0.02),
            opacity: 0.9,
        }
    }
}

/// The metrics of various terminal geometry. Dependent on font.
#[derive(Clone)]
struct TerminalMetrics {
    /// The size of each terminal cell.
    cell_size: Vec2,

    /// The metrics of each font.
    fonts: FontSet<FaceMetrics>,

    /// The baseline offset of each font type.
    baselines: FontSet<f32>,
}

impl TerminalMetrics {
    fn new(fonts: FontSet<&MsdfAtlas>) -> Self {
        let font_metrics = fonts.map(FaceMetrics::new);
        let cell_size = Vec2::new(font_metrics.regular.width, font_metrics.regular.height);
        let get_baseline = |font: &FaceMetrics| (cell_size.y - font.height) / 2.0 + font.ascender;
        let baselines = font_metrics.as_ref().map(get_baseline);

        Self {
            fonts: font_metrics,
            cell_size,
            baselines,
        }
    }
}

/// The configuration to initialize a terminal with.
#[derive(Clone, Debug, Default, Reflect)]
pub struct TerminalConfig {
    /// The terminal's initial space.
    pub space: TerminalSpace,

    /// The terminal's [FontSet].
    pub fonts: FontSet<Handle<MsdfAtlas>>,

    /// Shell options.
    ///
    /// [`None`] will use the default shell.
    pub shell: Option<Shell>,

    /// The working directory of this terminal's child process.
    pub working_directory: Option<PathBuf>,

    /// Extra environment variables.
    pub env: HashMap<String, String>,
}

/// Shell options.
#[derive(Clone, Debug, Default, Reflect)]
pub struct Shell {
    /// Path to a shell program to run on startup.
    pub program: String,

    /// Arguments passed to shell.
    pub args: Vec<String>,
}

#[derive(Component, TypePath)]
pub struct Terminal {
    /// The fonts to render this terminal with.
    fonts: FontSet<Handle<MsdfAtlas>>,

    /// The metrics of the selected font. Initialized when fonts are available.
    /// Set to `None` when fonts are changed.
    metrics: Option<TerminalMetrics>,

    /// This terminal's world space geometry.
    space: TerminalSpace,

    /// The current size of this terminal's grid.
    grid_size: UVec2,

    /// The internal handle to the terminal.
    term: Arc<FairMutex<Term<Listener>>>,

    /// A sender to this terminal's event loop.
    event_tx: EventLoopSender,

    /// A receiver from the terminal's child.
    event_rx: Receiver<ChildEvent>,

    /// The current configured set of colors this terminal uses.
    colors: Colors,

    /// The background image containing the colors of the background cells.
    ///
    /// Lazily initialized to point to an actual image on first use.
    bg: Option<Handle<Image>>,
}

impl Terminal {
    /// Creates a new terminal.
    pub fn new(config: TerminalConfig) -> Self {
        // default the grid size to 80x40 until font metrics can be obtained
        let grid_size = UVec2::new(80, 40);

        // create a channel for child events to this terminal
        let (sender, event_rx) = flume::unbounded();
        let listener = Listener { sender };

        // configure terminal
        let term_config = Config::default();

        // pass the size of the terminal grid
        let term_size = TermSize::new(grid_size.x as usize, grid_size.y as usize);

        // create the terminal helper
        let term = Arc::new(FairMutex::new(Term::new(
            term_config,
            &term_size,
            listener.clone(),
        )));

        // configure child
        let options = Options {
            hold: false, // don't hang upon child death
            shell: config.shell.as_ref().map(|shell| {
                alacritty_terminal::tty::Shell::new(shell.program.clone(), shell.args.clone())
            }),
            working_directory: config.working_directory,
            env: config.env,
        };

        // define initial child PTY size; ignoring floating point units
        let size = WindowSize {
            num_lines: grid_size.y as u16,
            num_cols: grid_size.x as u16,
            cell_width: 1,
            cell_height: 1,
        };

        // actually spawn the child process
        // TODO: how to handle window ID?
        let pty = alacritty_terminal::tty::new(&options, size, 0).unwrap();

        // set up event loop to forward us child's events
        let event_loop = EventLoop::new(term.clone(), listener, pty, false, false).unwrap();
        let event_tx = event_loop.channel();
        let _ = event_loop.spawn();

        let c = |r, g, b| Rgb { r, g, b };

        let palette = [
            (0x0, c(0, 0, 0)),         // black
            (0x1, c(187, 0, 0)),       // red
            (0x2, c(0, 187, 0)),       // green
            (0x3, c(187, 187, 0)),     // yellow
            (0x4, c(0, 0, 187)),       // blue
            (0x5, c(187, 0, 187)),     // magenta
            (0x6, c(0, 187, 187)),     // cyan
            (0x7, c(187, 187, 187)),   // white
            (0x8, c(85, 85, 85)),      // bright black
            (0x9, c(255, 85, 85)),     // bright red
            (0xA, c(85, 255, 85)),     // bright green
            (0xB, c(255, 255, 85)),    // bright yellow
            (0xC, c(85, 85, 255)),     // bright blue
            (0xD, c(255, 85, 255)),    // bright magenta
            (0xE, c(85, 255, 255)),    // bright cyan
            (0xF, c(255, 255, 255)),   // bright white
            (0x100, c(255, 255, 255)), // foreground
            (0x101, c(0, 0, 0)),       // background
        ];

        let mut colors = Colors::default();
        for (idx, rgb) in palette {
            colors[idx] = Some(rgb);
        }

        Self {
            fonts: config.fonts,
            metrics: None,
            colors,
            space: config.space,
            term,
            event_tx,
            event_rx,
            grid_size,
            bg: None,
        }
    }

    /// Dynamically update the fonts used by this terminal.
    pub fn set_fonts(&mut self, fonts: FontSet<Handle<MsdfAtlas>>) {
        self.fonts = fonts;
        self.metrics = None; // recalculate metrics accordingly
    }

    /// Updates the space occupied by this terminal.
    pub fn set_space(&mut self, space: TerminalSpace) {
        self.space = space;
    }

    /// Helper function to send a logical key's corresponding input sequence to this terminal.
    pub fn send_key(&self, key: &Key) {
        // TODO: handle modifiers too

        use Key::*;
        let input = match key {
            // not a &'static str, so catch and return early
            Character(c) => {
                self.send_input(&c.to_string());
                return;
            }
            Space => " ",
            Escape => "\x1b",
            Enter => "\r",
            Tab => "\t",
            Backspace => "\x7f",
            ArrowUp => "\x1b[A",
            ArrowDown => "\x1b[B",
            ArrowRight => "\x1b[C",
            ArrowLeft => "\x1b[D",
            Home => "\x1b[1~",
            Insert => "\x1b[2~",
            Delete => "\x1b[3~",
            End => "\x1b[4~",
            PageUp => "\x1b[5~",
            PageDown => "\x1b[6~",
            other => {
                warn!("unhandled terminal input logical key: {other:?}");
                return;
            }
        };

        self.send_input(input);
    }

    /// Sends keyboard input to this terminal.
    pub fn send_input(&self, input: &str) {
        let _ = self
            .event_tx
            .send(Msg::Input(input.as_bytes().to_vec().into()));
    }
}

/// A kind of font used by a terminal.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FontStyle {
    Regular,
    Italic,
    Bold,
    BoldItalic,
}

impl FontStyle {
    /// Convert from `alacritty_terminal`'s grid cell flags.
    pub fn from_cell_flags(flags: Flags) -> Self {
        if flags.contains(Flags::BOLD_ITALIC) {
            Self::BoldItalic
        } else if flags.contains(Flags::ITALIC) {
            Self::Italic
        } else if flags.contains(Flags::BOLD) {
            Self::Bold
        } else {
            Self::Regular
        }
    }
}

/// Generic container for all font faces used in a terminal. Eases
/// the writing of code manipulating all faces at once.
#[derive(Clone, Debug, Default, Reflect)]
pub struct FontSet<T> {
    pub regular: T,
    pub italic: T,
    pub bold: T,
    pub bold_italic: T,
}

impl<T: Clone> FontSet<T> {
    pub fn splat(font: T) -> Self {
        Self {
            regular: font.clone(),
            italic: font.clone(),
            bold: font.clone(),
            bold_italic: font,
        }
    }
}

impl<T> FontSet<Option<T>> {
    pub fn all(self) -> Option<FontSet<T>> {
        if let FontSet {
            regular: Some(regular),
            italic: Some(italic),
            bold: Some(bold),
            bold_italic: Some(bold_italic),
        } = self
        {
            Some(FontSet {
                regular,
                italic,
                bold,
                bold_italic,
            })
        } else {
            None
        }
    }
}

impl<T> FontSet<T> {
    pub fn map<O>(self, f: impl Fn(T) -> O) -> FontSet<O> {
        FontSet {
            regular: f(self.regular),
            italic: f(self.italic),
            bold: f(self.bold),
            bold_italic: f(self.bold_italic),
        }
    }

    pub fn for_each(self, mut f: impl FnMut(T)) {
        f(self.regular);
        f(self.italic);
        f(self.bold);
        f(self.bold_italic);
    }

    pub fn get(&self, style: FontStyle) -> &T {
        match style {
            FontStyle::Regular => &self.regular,
            FontStyle::Italic => &self.italic,
            FontStyle::Bold => &self.bold,
            FontStyle::BoldItalic => &self.bold_italic,
        }
    }

    pub fn get_mut(&mut self, style: FontStyle) -> &mut T {
        match style {
            FontStyle::Regular => &mut self.regular,
            FontStyle::Italic => &mut self.italic,
            FontStyle::Bold => &mut self.bold,
            FontStyle::BoldItalic => &mut self.bold_italic,
        }
    }

    pub fn zip<O>(self, other: FontSet<O>) -> FontSet<(T, O)> {
        FontSet {
            regular: (self.regular, other.regular),
            italic: (self.italic, other.italic),
            bold: (self.bold, other.bold),
            bold_italic: (self.bold_italic, other.bold_italic),
        }
    }

    pub fn as_ref(&self) -> FontSet<&T> {
        FontSet {
            regular: &self.regular,
            italic: &self.italic,
            bold: &self.bold,
            bold_italic: &self.bold_italic,
        }
    }

    pub fn as_mut(&mut self) -> FontSet<&mut T> {
        FontSet {
            regular: &mut self.regular,
            italic: &mut self.italic,
            bold: &mut self.bold,
            bold_italic: &mut self.bold_italic,
        }
    }
}

#[derive(Clone)]
struct Listener {
    sender: Sender<ChildEvent>,
}

impl EventListener for Listener {
    fn send_event(&self, event: ChildEvent) {
        let _ = self.sender.send(event);
    }
}

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone, Default)]
struct BgMaterial {
    #[texture(0)]
    #[sampler(1)]
    bg: Handle<Image>,
}

impl Material for BgMaterial {
    fn fragment_shader() -> ShaderRef {
        "embedded://bevy_alacritty/bg.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        AlphaMode::Premultiplied
    }
}

#[derive(Clone)]
struct FaceMetrics {
    ascender: f32,
    width: f32,
    height: f32,
    strikeout_pos: f32,
    strikeout_width: f32,
    underline_pos: f32,
    underline_width: f32,
}

impl FaceMetrics {
    fn new(atlas: &MsdfAtlas) -> Self {
        let face = atlas.face.as_face_ref();
        let units_per_em = face.units_per_em() as f32;
        let ascender = face.ascender() as f32 / units_per_em;
        let height = face.height() as f32 / units_per_em;
        let descender = face.descender() as f32 / units_per_em;
        let height = height.max(ascender + descender);
        let width = face
            .glyph_index('M')
            .and_then(|id| face.glyph_hor_advance(id))
            .map(|adv| adv as f32 / units_per_em)
            .unwrap_or(1.0);

        let default_width = 0.06;

        let (strikeout_pos, strikeout_width) = face
            .strikeout_metrics()
            .map(|m| (Vec2::new(m.position as f32, m.thickness as f32) / units_per_em).into())
            .unwrap_or((height / 2.0, default_width));

        let (underline_pos, underline_width) = face
            .underline_metrics()
            .map(|m| (Vec2::new(m.position as f32, m.thickness as f32) / units_per_em).into())
            .unwrap_or((height / 2.0, default_width));

        Self {
            ascender,
            height,
            width,
            strikeout_pos,
            strikeout_width,
            underline_pos,
            underline_width,
        }
    }
}

/// Converts a terminal color code to RGB.
pub fn color_to_rgb(colors: &Colors, color: AnsiColor) -> Rgb {
    match color {
        AnsiColor::Named(name) => colors[name].unwrap_or(Rgb {
            r: 0xff,
            g: 0x00,
            b: 0xff,
        }),
        AnsiColor::Spec(rgb) => rgb,
        AnsiColor::Indexed(index) => {
            if let Some(color) = colors[index as usize] {
                color
            } else if let Some(gray) = index.checked_sub(232) {
                let value = gray * 10 + 8;
                Rgb {
                    r: value,
                    g: value,
                    b: value,
                }
            } else if let Some(cube_idx) = index.checked_sub(16) {
                let r = cube_idx / 36;
                let g = (cube_idx / 6) % 6;
                let b = cube_idx % 6;

                let c = |c| {
                    if c == 0 {
                        0
                    } else {
                        c * 40 + 55
                    }
                };

                Rgb {
                    r: c(r),
                    g: c(g),
                    b: c(b),
                }
            } else {
                Rgb {
                    r: 0xff,
                    g: 0x00,
                    b: 0xff,
                }
            }
        }
    }
}

/// Helper function to convert an Alacritty color to a Bevy color.
pub fn color_to_bevy(colors: &Colors, color: AnsiColor) -> Color {
    let rgb = color_to_rgb(colors, color);
    Color::rgb_u8(rgb.r, rgb.g, rgb.b)
}
