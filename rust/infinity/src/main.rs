use ggez;
use ggez::event;
use ggez::graphics;
use ggez::nalgebra as na;
use na::{Rotation2};

struct MainState {
    pos_x: f32,
}

impl MainState {
    fn new() -> ggez::GameResult<MainState> {
        let s = MainState { pos_x: 0.0 };
        Ok(s)
    }
}

impl event::EventHandler for MainState {
    fn update(&mut self, _ctx: &mut ggez::Context) -> ggez::GameResult {
        self.pos_x = self.pos_x % 800.0 + 1.0;
        Ok(())
    }

    fn draw(&mut self, ctx: &mut ggez::Context) -> ggez::GameResult {
        graphics::clear(ctx, [0.1, 0.2, 0.3, 1.0].into());

        // Draw lines
        let lines_num = 100;
        let origin: na::Point2<_> = [200., 200.].into();

        for line_idx in 0..lines_num {
            let whole_angle: f32 = 2. * <f32 as na::RealField>::pi();
            let line_idx = line_idx as f32;
            let lines_num = lines_num as f32;
            let line_angle = (whole_angle / lines_num) * line_idx;
            let line_versor = Rotation2::new(line_angle) * na::Vector2::new(1., 0.);

            let line_length = 100.;

            let line = graphics::Mesh::new_line(
                ctx,
                &[origin, origin + (line_length * line_versor)],
                1.,
                graphics::WHITE,
            )?;
            graphics::draw(ctx, &line, (na::Point2::new(0.0, 0.0),))?;
        }

        graphics::present(ctx)?;
        Ok(())
    }
}

pub fn main() -> ggez::GameResult { 
    let cb = ggez::ContextBuilder::new("super_simple", "ggez");
    let (ctx, event_loop) = &mut cb.build()?;
    let state = &mut MainState::new()?;
    event::run(ctx, event_loop, state)
}

