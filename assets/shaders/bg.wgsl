// SPDX-License-Identifier: Apache-2.0

#import bevy_pbr::forward_io::VertexOutput

@group(2) @binding(0) var bg_texture: texture_2d<f32>;
@group(2) @binding(1) var bg_sampler: sampler;

@fragment
fn fragment(mesh: VertexOutput) -> @location(0) vec4<f32> {
    // the "pixel art upscaling" method comes from here:
    // https://www.youtube.com/watch?v=d6tp43wZqps

    // retrieve the texture size as a local variable
    let texture_size = vec2<f32>(textureDimensions(bg_texture));

    // box filter size in texel units
    let box_size = clamp(fwidth(mesh.uv) * texture_size, vec2<f32>(1e-5), vec2<f32>(1.0));

    // scale uv by texture size to get texel coordinate
    let tx = mesh.uv * texture_size - 0.5 * box_size;

    // compute offset for pixel-sized box filter
    let tx_offset = smoothstep(vec2<f32>(1.0) - box_size, vec2<f32>(1.0), fract(tx));

    // compute bilinear sample uv coordinates
    let uv = (floor(tx) + 0.5 + tx_offset) / texture_size;

    // sample the texture
    return textureSampleGrad(bg_texture, bg_sampler, uv, dpdx(mesh.uv), dpdy(mesh.uv));
}
