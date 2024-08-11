// SPDX-License-Identifier: Apache-2.0

use bevy::{
    app::AppExit,
    input::{keyboard::KeyboardInput, ButtonState},
    prelude::*,
};
use bevy_alacritty::*;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(TerminalPlugin)
        .add_systems(Startup, setup)
        .add_systems(Update, input)
        .run();
}

fn setup(mut commands: Commands, assets: ResMut<AssetServer>) {
    // load fonts
    let fonts = FontSet::splat(assets.load("mononoki-Regular.ttf"));

    // terminal
    commands.spawn((
        Terminal::new(TerminalConfig { fonts, ..default() }),
        TransformBundle::default(),
        VisibilityBundle::default(),
    ));

    // camera
    commands.spawn(Camera3dBundle {
        transform: Transform::from_xyz(0.1, 0.1, 1.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
}

fn input(
    mut exit: EventWriter<AppExit>,
    mut kbd: EventReader<KeyboardInput>,
    query: Query<&Terminal>,
) {
    let Ok(terminal) = query.get_single() else {
        exit.send(AppExit::Success);
        return;
    };

    for ev in kbd.read() {
        if ev.state == ButtonState::Released {
            continue;
        }

        terminal.send_key(&ev.logical_key);
    }
}
