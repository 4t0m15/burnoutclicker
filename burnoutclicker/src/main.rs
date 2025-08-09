use bevy::prelude::*;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use std::f32::consts::TAU;

const WINDOW_W: f32 = 1280.0;
const WINDOW_H: f32 = 800.0;

#[derive(States, Debug, Hash, PartialEq, Eq, Clone, Default)]
enum GameState {
    #[default]
    Playing,
    Won,
}

#[derive(Resource, Clone)]
struct Config {
    base_rate: f32,
    noise_strength: f32,
    plateau_prob_per_s: f32,
    setback_prob_per_s: f32,
    setback_strength: f32,
    plateau_min_s: f32,
    plateau_max_s: f32,
    growth_increment_base: f32,
    growth_increment_decay: f32,
    requirement_scale: f32,
    max_growths: u32,
    final_radius: f32,
    grow_anim_s: f32,
    halo_s: f32,
    boost_strength: f32,
    boost_s: f32,
    jitter_px: f32,
    bar_w: f32,
    bar_h: f32,
    circle_start_radius: f32,
    circle_height: f32,
    vignette_radius: f32,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            base_rate: 0.05,
            noise_strength: 0.55,
            plateau_prob_per_s: 0.08,
            setback_prob_per_s: 0.03,
            setback_strength: 0.06,
            plateau_min_s: 0.4,
            plateau_max_s: 1.6,
            growth_increment_base: 36.0,
            growth_increment_decay: 0.83,
            requirement_scale: 1.18,
            max_growths: 9,
            final_radius: 210.0,
            grow_anim_s: 0.45,
            halo_s: 0.7,
            boost_strength: 0.85,
            boost_s: 0.28,
            jitter_px: 2.0,
            bar_w: 940.0,
            bar_h: 22.0,
            circle_start_radius: 70.0,
            circle_height: 110.0,
            vignette_radius: 1100.0,
        }
    }
}

#[derive(Resource)]
struct Progress {
    value: f32, // 0..1
    inst_speed: f32,
}

#[derive(Resource)]
struct Growth {
    count: u32,
    effort_mul: f32,
}

#[derive(Resource)]
struct CircleAnim {
    current_radius: f32,
    start_radius: f32,
    target_radius: f32,
    t: f32,
    active: bool,
    // subtle breathing
    breathe_t: f32,
    // halo pulse
    halo_t: f32,
    // boost pulse
    boost_t: f32,
}

#[derive(Default, Resource)]
struct WinFade {
    t: f32,
    // 0..1 for main fade; after some time black falls if image loaded
    image_loaded: bool,
}

#[derive(Resource)]
struct NoiseState {
    t: f32,
    p2: f32,
    p3: f32,
    rng: Lcg,
}

#[derive(Resource, Default)]
struct EffectState {
    mode: EffectMode,
    timer: f32,
    strength: f32,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
enum EffectMode {
    #[default]
    None,
    Plateau,
    Setback,
}

// Simple deterministic LCG RNG
#[derive(Clone)]
struct Lcg {
    state: u64,
}
impl Lcg {
    fn with_seed(seed: u64) -> Self {
        let s = if seed == 0 { 0x9E37_79B9_7F4A_7C15 } else { seed };
        Self { state: s }
    }
    fn next_u32(&mut self) -> u32 {
        self.state = self.state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1);
        (self.state >> 16) as u32
    }
    fn next_f32(&mut self) -> f32 {
        (self.next_u32() as f32) / (u32::MAX as f32)
    }
    fn range_f32(&mut self, lo: f32, hi: f32) -> f32 {
        lo + (hi - lo) * self.next_f32()
    }
}

#[derive(Component)]
struct SelfCircle;

#[derive(Component)]
struct CircleHalo;

#[derive(Component)]
struct BarFill;

#[derive(Component)]
struct BarTrack;

#[derive(Component)]
struct Vignette;

#[derive(Component)]
struct WinOverlayBlack;

#[derive(Component)]
struct WinOverlayImage {
    handle: Handle<Image>,
}

fn main() {
    App::new()
        .insert_resource(ClearColor(Color::srgb(0.03, 0.03, 0.035)))
        .insert_resource(Msaa::Off)
        .insert_resource(Config::default())
        .insert_resource(Progress { value: 0.0, inst_speed: 0.0 })
        .insert_resource(Growth { count: 0, effort_mul: 1.0 })
        .insert_resource(CircleAnim {
            current_radius: Config::default().circle_start_radius,
            start_radius: Config::default().circle_start_radius,
            target_radius: Config::default().circle_start_radius,
            t: 0.0,
            active: false,
            breathe_t: 0.0,
            halo_t: 0.0,
            boost_t: 0.0,
        })
        .insert_resource(NoiseState {
            t: 0.0,
            p2: 1.2345,
            p3: 4.5678,
            rng: Lcg::with_seed(
                // reproducible but varied per run
                (std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .subsec_nanos() as u64)
                    ^ 0xA5A5_7B7B_C3C3_DD55,
            ),
        })
        .insert_resource(EffectState::default())
        .insert_resource(WinFade::default())
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                resolution: (WINDOW_W, WINDOW_H).into(),
                title: String::new(),
                resizable: true,
                ..default()
            }),
            ..default()
        }))
        .add_state::<GameState>()
        // Setup
        .add_systems(Startup, setup)
        // Playing
        .add_systems(
            Update,
            (
                input_boost_and_quit,
                update_progress_and_bar,
                check_growth_and_win,
                animate_circle_and_halo,
            )
                .run_if(in_state(GameState::Playing)),
        )
        // Win fade
        .add_systems(OnEnter(GameState::Won), begin_win)
        .add_systems(Update, win_fade.run_if(in_state(GameState::Won)))
        .run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    asset_server: Res<AssetServer>,
    cfg: Res<Config>,
) {
    commands.spawn(Camera2dBundle::default());

    // Vignette: subtle large circle behind everything
    let vignette_mesh: Mesh2dHandle = meshes.add(bevy::math::primitives::Circle { radius: 1.0 }.into()).into();
    let vignette_color = Color::srgba(0.0, 0.0, 0.0, 0.12);
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: vignette_mesh.clone(),
            material: materials.add(vignette_color),
            transform: Transform {
                translation: Vec3::new(0.0, 0.0, -50.0),
                scale: Vec3::splat(cfg.vignette_radius),
                ..default()
            },
            ..default()
        },
        Vignette,
    ));

    // Bar track (ground-like)
    let rect_mesh: Mesh2dHandle = meshes
        .add(bevy::math::primitives::Rectangle::from_size(Vec2::new(1.0, 1.0)).into())
        .into();
    let track_color = Color::srgba(0.05, 0.18, 0.07, 1.0);
    let fill_color = Color::srgba(0.10, 0.65, 0.18, 1.0);

    let bar_y = -WINDOW_H * 0.5 + 80.0;
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: rect_mesh.clone(),
            material: materials.add(track_color),
            transform: Transform {
                translation: Vec3::new(0.0, bar_y, 0.0),
                scale: Vec3::new(cfg.bar_w, cfg.bar_h, 1.0),
                ..default()
            },
            ..default()
        },
        BarTrack,
    ));

    // Bar fill (starts empty)
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: rect_mesh.clone(),
            material: materials.add(fill_color),
            transform: Transform {
                translation: Vec3::new(-cfg.bar_w * 0.5, bar_y, 1.0), // will be adjusted per frame
                scale: Vec3::new(0.0, cfg.bar_h - 4.0, 1.0),
                ..default()
            },
            ..default()
        },
        BarFill,
    ));

    // Circle ("the self")
    let circle_mesh: Mesh2dHandle = meshes.add(bevy::math::primitives::Circle { radius: 1.0 }.into()).into();
    let circle_y = bar_y + cfg.circle_height;
    let blue = Color::srgba(0.20, 0.50, 0.95, 1.0);
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: circle_mesh.clone(),
            material: materials.add(blue),
            transform: Transform {
                translation: Vec3::new(0.0, circle_y, 2.0),
                scale: Vec3::splat(cfg.circle_start_radius),
                ..default()
            },
            ..default()
        },
        SelfCircle,
    ));

    // Halo (soft pulse on growth)
    let halo_color = Color::srgba(0.30, 0.65, 1.0, 0.0);
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: circle_mesh,
            material: materials.add(halo_color),
            transform: Transform {
                translation: Vec3::new(0.0, circle_y, 1.5),
                scale: Vec3::splat(cfg.circle_start_radius * 1.05),
                ..default()
            },
            ..default()
        },
        CircleHalo,
    ));

    // Win overlays (black and image; both start transparent)
    let black_color = Color::srgba(0.0, 0.0, 0.0, 0.0);
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: meshes
                .add(bevy::math::primitives::Rectangle::from_size(Vec2::new(1.0, 1.0)).into())
                .into(),
            material: materials.add(black_color),
            transform: Transform {
                translation: Vec3::new(0.0, 0.0, 1000.0),
                scale: Vec3::new(WINDOW_W * 2.0, WINDOW_H * 2.0, 1.0),
                ..default()
            },
            ..default()
        },
        WinOverlayBlack,
    ));

    let handle: Handle<Image> = asset_server.load("win_screen.png");
    commands.spawn((
        SpriteBundle {
            texture: handle.clone(),
            sprite: Sprite {
                custom_size: Some(Vec2::new(WINDOW_W * 2.0, WINDOW_H * 2.0)),
                color: Color::srgba(1.0, 1.0, 1.0, 0.0),
                ..default()
            },
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 1001.0)),
            visibility: Visibility::Hidden,
            ..default()
        },
        WinOverlayImage { handle },
    ));
}

fn input_boost_and_quit(
    kb: Res<ButtonInput<KeyCode>>,
    mouse: Res<ButtonInput<MouseButton>>,
    mut anim: ResMut<CircleAnim>,
    mut exit: EventWriter<AppExit>,
) {
    if kb.just_pressed(KeyCode::Escape) || kb.just_pressed(KeyCode::KeyQ) {
        exit.send(AppExit);
    }
    if kb.just_pressed(KeyCode::Space) || mouse.just_pressed(MouseButton::Left) {
        anim.boost_t = 1.0;
    }
}

fn update_progress_and_bar(
    time: Res<Time>,
    cfg: Res<Config>,
    mut progress: ResMut<Progress>,
    mut growth: ResMut<Growth>,
    mut noise: ResMut<NoiseState>,
    mut effect: ResMut<EffectState>,
    mut q_fill: Query<(&mut Transform, &mut Handle<ColorMaterial>), With<BarFill>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    let dt = time.delta_seconds();

    // Smooth noise from combined sines + slow phase drift
    noise.t += dt;
    noise.p2 += dt * 0.043;
    noise.p3 += dt * 0.017;
    let s1 = (noise.t * 0.37 * TAU).sin();
    let s2 = (noise.t * 0.19 * TAU + noise.p2 * 0.5).sin();
    let s3 = (noise.t * 0.09 * TAU + noise.p3 * 0.8).sin();
    let smooth_noise = (0.6 * s1 + 0.3 * s2 + 0.2 * s3) * 0.8;

    // Randomly trigger plateaus/setbacks
    if effect.mode == EffectMode::None {
        if noise.rng.next_f32() < cfg.plateau_prob_per_s * dt {
            effect.mode = EffectMode::Plateau;
            effect.timer = noise.rng.range_f32(cfg.plateau_min_s, cfg.plateau_max_s);
            effect.strength = noise.rng.range_f32(0.5, 0.85); // percent reduction
        } else if noise.rng.next_f32() < cfg.setback_prob_per_s * dt {
            effect.mode = EffectMode::Setback;
            effect.timer = noise.rng.range_f32(0.15, 0.35);
            effect.strength = cfg.setback_strength * noise.rng.range_f32(0.6, 1.4);
        }
    } else {
        effect.timer -= dt;
        if effect.timer <= 0.0 {
            effect.mode = EffectMode::None;
            effect.timer = 0.0;
            effect.strength = 0.0;
        }
    }

    // Base speed with diminishing returns via effort multiplier
    let base = cfg.base_rate / growth.effort_mul;
    let mut speed = base * (1.0 + cfg.noise_strength * smooth_noise);

    // Plateau reduces, setback can subtract
    match effect.mode {
        EffectMode::Plateau => {
            speed *= (1.0 - effect.strength).clamp(0.02, 1.0);
        }
        EffectMode::Setback => {
            speed -= effect.strength * base;
        }
        EffectMode::None => {}
    }

    // Apply boost as a temporary multiplier (handled and decayed in animate system)
    // Here we only read progress.inst_speed after animate updates; instead we nudge via a quick envelope using inst_speed placeholder.
    // We'll adjust brightness using computed normalized speed below.

    // Advance progress, clamp with tiny setback allowance
    let prev = progress.value;
    progress.value = (progress.value + speed * dt).clamp(0.0, 1.0);
    progress.inst_speed = (progress.value - prev) / dt;

    // Update bar fill transform and subtle brightness
    if let Ok((mut t, mat_handle)) = q_fill.get_single_mut() {
        let w = cfg.bar_w;
        let current_w = w * progress.value;
        // left anchor
        t.translation.x = -w * 0.5 + current_w * 0.5;
        t.scale.x = current_w.max(0.0);

        if let Some(mat) = materials.get_mut(&*mat_handle) {
            let normalized = (progress.inst_speed / base).clamp(0.5, 1.6);
            let brighten = (normalized - 0.5) / (1.6 - 0.5); // 0..1
            let base_col = Color::srgba(0.10, 0.65, 0.18, 1.0);
            // Lerp to brighter/darker subtly
            let to_white = 0.10 * brighten;
            let to_black = 0.06 * (1.0 - brighten);
            let mut col = base_col;
            // towards white
            col = Color::srgba(
                (col.r() * (1.0 - to_white) + 1.0 * to_white).min(1.0),
                (col.g() * (1.0 - to_white) + 1.0 * to_white).min(1.0),
                (col.b() * (1.0 - to_white) + 1.0 * to_white).min(1.0),
                1.0,
            );
            // towards black slightly when slow
            col = Color::srgba(
                (col.r() * (1.0 - to_black)).max(0.0),
                (col.g() * (1.0 - to_black)).max(0.0),
                (col.b() * (1.0 - to_black)).max(0.0),
                1.0,
            );
            mat.color = col;
        }
    }
}

fn check_growth_and_win(
    time: Res<Time>,
    cfg: Res<Config>,
    mut progress: ResMut<Progress>,
    mut growth: ResMut<Growth>,
    mut anim: ResMut<CircleAnim>,
    mut next_state: ResMut<NextState<GameState>>,
) {
    if progress.value >= 1.0 {
        progress.value = 0.0;
        growth.count = growth.count.saturating_add(1);
        growth.effort_mul *= cfg.requirement_scale;

        // Diminishing increment
        let inc = cfg.growth_increment_base * cfg.growth_increment_decay.powf(growth.count as f32 - 1.0);
        anim.start_radius = anim.current_radius;
        anim.target_radius = (anim.current_radius + inc).min(cfg.final_radius);
        anim.t = 0.0;
        anim.active = true;
        anim.halo_t = 1.0;

        // Win condition
        if growth.count >= cfg.max_growths || anim.target_radius >= cfg.final_radius - 0.5 {
            // Small delay before starting fade for a breath
            anim.halo_t = 1.0;
            // begin after a brief moment
            // Use timer via Win state
            next_state.set(GameState::Won);
        }
    }

    // keep breathe time moving
    anim.breathe_t += time.delta_seconds();
}

fn animate_circle_and_halo(
    time: Res<Time>,
    cfg: Res<Config>,
    mut anim: ResMut<CircleAnim>,
    mut noise: ResMut<NoiseState>,
    progress: Res<Progress>,
    mut q_circle: Query<(&mut Transform, &mut Handle<ColorMaterial>), (With<SelfCircle>, Without<CircleHalo>)>,
    mut q_halo: Query<(&mut Transform, &mut Handle<ColorMaterial>), With<CircleHalo>>,
    mut q_fill_mat: Query<&mut Handle<ColorMaterial>, With<BarFill>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    let dt = time.delta_seconds();

    // Easing on radius growth
    if anim.active {
        anim.t = (anim.t + dt / cfg.grow_anim_s).min(1.0);
        let t = ease_out_cubic(anim.t);
        anim.current_radius = anim.start_radius + (anim.target_radius - anim.start_radius) * t;
        if anim.t >= 1.0 {
            anim.active = false;
        }
    }

    // Boost decay
    if anim.boost_t > 0.0 {
        anim.boost_t = (anim.boost_t - dt / cfg.boost_s).max(0.0);
    }

    // Update circle transform (scale + subtle breathing + jitter on slowdowns)
    let breathe = 1.0 + 0.008 * (anim.breathe_t * TAU * 0.14).sin();
    let boost_pulse = if anim.boost_t > 0.0 {
        1.0 + 0.025 * ease_out_cubic(anim.boost_t)
    } else {
        1.0
    };
    let mut jitter_x = 0.0;
    let mut jitter_y = 0.0;
    let slow = (progress.inst_speed / (cfg.base_rate / 1.0)).clamp(0.0, 1.0) < 0.85;
    if slow {
        jitter_x = (noise.rng.next_f32() - 0.5) * 2.0 * cfg.jitter_px;
        jitter_y = (noise.rng.next_f32() - 0.5) * 2.0 * cfg.jitter_px;
    } else {
        // gentle micro wobble
        jitter_x = 0.5 * (anim.breathe_t * TAU * 0.27).sin();
        jitter_y = 0.5 * (anim.breathe_t * TAU * 0.31).cos();
    }

    if let Ok((mut t, _mat)) = q_circle.get_single_mut() {
        let base_y = -WINDOW_H * 0.5 + 80.0 + cfg.circle_height;
        t.translation.x = jitter_x;
        t.translation.y = base_y + jitter_y;
        t.scale = Vec3::splat(anim.current_radius * breathe * boost_pulse);
    }

    // Halo pulse
    if let Ok((mut t_halo, mat_handle)) = q_halo.get_single_mut() {
        if anim.halo_t > 0.0 {
            anim.halo_t = (anim.halo_t - dt / cfg.halo_s).max(0.0);
            let k = 1.0 - anim.halo_t;
            let scale_mul = 1.05 + 0.4 * k;
            t_halo.scale = Vec3::splat(anim.current_radius * scale_mul);
            if let Some(mat) = materials.get_mut(&*mat_handle) {
                mat.color.set_a((1.0 - k).powf(0.6) * 0.4); // fade out
            }
        } else if let Some(mat) = materials.get_mut(&*mat_handle) {
            mat.color.set_a(0.0);
            t_halo.scale = Vec3::splat(anim.current_radius * 1.05);
        }
        t_halo.translation.x = jitter_x * 0.6;
        t_halo.translation.y = (-WINDOW_H * 0.5 + 80.0 + cfg.circle_height) + jitter_y * 0.6;
    }

    // Subtle bar glow on boost (brief brighten)
    if anim.boost_t > 0.0 {
        if let Ok(handle) = q_fill_mat.get_single_mut() {
            if let Some(mat) = materials.get_mut(&*handle) {
                let k = ease_out_cubic(anim.boost_t).min(1.0);
                let c = mat.color;
                mat.color = Color::srgba(
                    (c.r() * (1.0 - 0.12 * k) + 1.0 * 0.12 * k).min(1.0),
                    (c.g() * (1.0 - 0.10 * k) + 1.0 * 0.10 * k).min(1.0),
                    (c.b() * (1.0 - 0.06 * k) + 1.0 * 0.06 * k).min(1.0),
                    1.0,
                );
            }
        }
    }
}

fn begin_win() {
    // Nothing needed right away; WinFade update will crossfade.
}

fn win_fade(
    time: Res<Time>,
    mut fade: ResMut<WinFade>,
    mut q_black: Query<(&mut Handle<ColorMaterial>, &mut Visibility), With<WinOverlayBlack>>,
    mut q_img: Query<(&WinOverlayImage, &mut Sprite, &mut Visibility)>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    assets: Res<AssetServer>,
) {
    let dt = time.delta_seconds();
    fade.t = (fade.t + dt).min(4.0);

    // Fade to black over ~0.9s, then ease in image over 1.6s, then fade black away
    let black_in = (fade.t / 0.9).clamp(0.0, 1.0);
    let img_in = ((fade.t - 0.4) / 1.6).clamp(0.0, 1.0);

    if let Ok((mat_handle, mut vis)) = q_black.get_single_mut() {
        if let Some(mat) = materials.get_mut(&*mat_handle) {
            let a = ease_in_out(black_in);
            mat.color.set_a(a);
        }
        *vis = Visibility::Visible;
    }

    if let Ok((win, mut sprite, mut vis)) = q_img.get_single_mut() {
        let loaded = assets.is_loaded_with_dependencies(&win.handle);
        if loaded && !fade.image_loaded {
            fade.image_loaded = true;
        }
        if fade.image_loaded {
            *vis = Visibility::Visible;
            sprite.color.set_a(ease_in_out(img_in));
            // Optionally reduce black slightly once image visible
            if let Ok((mat_handle, _)) = q_black.get_single_mut() {
                if let Some(mat) = materials.get_mut(&*mat_handle) {
                    let a = (1.0 - img_in * 0.9).clamp(0.0, 1.0);
                    mat.color.set_a(a);
                }
            }
        } else {
            *vis = Visibility::Hidden;
        }
    }
}

// Easing helpers
fn ease_out_cubic(t: f32) -> f32 {
    let u = (1.0 - t).clamp(0.0, 1.0);
    1.0 - u * u * u
}
fn ease_in_out(t: f32) -> f32 {
    let t = t.clamp(0.0, 1.0);
    if t < 0.5 {
        4.0 * t * t * t
    } else {
        1.0 - (-2.0 * t + 2.0).powf(3.0) / 2.0
    }
}