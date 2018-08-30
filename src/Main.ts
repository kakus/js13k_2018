// math
namespace qm
{
    export class vec { 
        constructor(
            public x: number,
            public y: number
        ) { }
    }
    
    export class cvec {
        constructor(
            public readonly x: number,
            public readonly y: number
        ) { }
    }
    
    export const up = new cvec(0, -1);
    export const down = new cvec(0, 1);
    export const left = new cvec(-1, 0);
    export const right = new cvec(1, 0);

    export function v(x = 0, y = 0) { return new vec(x, y); }
    export function vc(a: vec) { return new vec(a.x, a.y); }
    export function eq(a: vec, b: vec) { return a.x == b.x && a.y == b.y; }
    export function add(a: vec, b: vec) { return v(a.x + b.x, a.y + b.y); }
    export function sub(a: vec, b: vec) { return v(a.x - b.x, a.y - b.y); }
    export function mul(a: vec, b: vec) { return v(a.x * b.x, a.y * b.y); }
    export function scale(a: vec, s: number) { return v(a.x * s, a.y * s); }
    export function cross(a: vec, b: vec) { return a.x * b.y - a.y * b.x; }
    export function sign(a: vec) { return v(Math.sign(a.x), Math.sign(a.y)); }
    export function mag_sqr(a: vec) { return a.x * a.x + a.y * a.y; }
    export function mag(a: vec) { return Math.sqrt(mag_sqr(a)); }
    export function unit(a: vec) { let m = mag(a); return scale(a, 1/m); }
    export function clamp_mag(a: vec, min: number, max: number)
    {
        let m = mag(a);
        return m < min ? scale(unit(a), min) :
               m > max ? scale(unit(a), max) :
                         a;
    }

    // general math
    export function clamp(x: number, min: number, max: number) { return Math.max(Math.min(x, max), min); }
    export function rnd(min = 0, max = 1) { return min + (max - min) * Math.random(); }

    export type aabb = [vec, vec];

    // a, b - any two points
    // return [center, half_size]
    export function make_aabb(a: vec, b: vec): aabb
    {
        let tl = v(Math.min(a.x, b.x), Math.min(a.y, b.y));
        let br = v(Math.max(a.x, b.x), Math.max(a.y, b.y));

        return [qm.scale(qm.add(tl, br), 0.5), qm.scale(qm.sub(br, tl), 0.5)];
    }

    // position, normal
    export type hit_result = [qm.vec, qm.vec];

    export function line_trace_aabb(start: qm.vec, end: qm.vec, aabb: qm.aabb): hit_result
    {
        let get_vertex = (i: number) => {
            let [center, ext] = aabb;
            const hx = [-ext.x, ext.x, ext.x, -ext.x];
            const hy = [-ext.y, -ext.y, ext.y, ext.y];
            return qm.v(center.x + hx[i % 4], center.y + hy[i % 4]);
        };

        const trace = qm.sub(end, start);
        let hits: qm.vec[] = new Array(4);

        for (let i = 0; i < 4; ++i)
        {
            let v1 = get_vertex(i), 
                v2 = get_vertex(i + 1);
            let edge = qm.sub(v2, v1);

            // if trace start point is above edge
            if (qm.cross(edge, qm.sub(start, v1)) <= 0 && 
            // and end trace is below line
                qm.cross(edge, qm.sub(end, v1)) >= 0)
            {
                // trace is vertical line
                if (trace.x == 0)
                {
                    // we can only colide against horizontal line
                    if (i == 0 || i == 2)
                    {
                        if (start.x >= Math.min(v1.x, v2.x) && start.x <= Math.max(v1.x, v2.x))
                        {
                            hits[i] = qm.v(start.x, v1.y);
                        }
                    }
                    continue;
                }

                const a = trace.y / trace.x;
                const b = start.y - a * start.x;

                // horizontal line
                if (i == 0 || i == 2)
                {
                    // we can't hit horizontal line with horizontal trace
                    if (a == 0) continue;

                    let y = v1.y;
                    let x = (y - b) / a;
                    
                    if (x >= Math.min(v1.x, v2.x) && x <= Math.max(v1.x, v2.x))
                    {
                        hits[i] = qm.v(x, y);
                    }
                }
                // vertical line
                else 
                {
                    let x = v1.x;
                    let y = a * x + b;

                    if (y >= Math.min(v1.y, v2.y) && y <= Math.max(v1.y, v2.y))
                    {
                        hits[i] = qm.v(x, y);
                    }
                }
            }
        }

        let best_dist = Number.MAX_VALUE;
        let best_index = -1;

        for (let i = 0; i < 4; ++i)
        {
            if (hits[i])
            {
                let dist = qm.mag_sqr(qm.sub(hits[i], start));
                if (dist < best_dist)
                {
                    best_dist = dist;
                    best_index = i;
                }
            }
        }

        if (best_index != -1)
        {
            const normal = [qm.up, qm.right, qm.down, qm.left];
            return [hits[best_index], normal[best_index]];
        }
    }

    export type mat = Float32Array;

    export function mat(m00 = 1, m10 = 0, m01 = 0, m11 = 1, tx = 0, ty = 0): mat
    {
        return new Float32Array([m00, m10, 0, m01, m11, 0, tx, ty, 1]);
    }

    export function transform(v: vec, m: mat): vec
    {
        return qm.v(v.x * m[0] + v.y * m[3] + m[6], 
                    v.x * m[1] + v.y * m[4] + m[7]);
    }

    export function mat_mul( a: mat, b: mat, out: mat ): void
    {
        var a00 = a[ 0 ], a01 = a[ 1 ], a02 = a[ 2 ],
            a10 = a[ 3 ], a11 = a[ 4 ], a12 = a[ 5 ],
            a20 = a[ 6 ], a21 = a[ 7 ], a22 = a[ 8 ],

            b00 = b[ 0 ], b01 = b[ 1 ], b02 = b[ 2 ],
            b10 = b[ 3 ], b11 = b[ 4 ], b12 = b[ 5 ],
            b20 = b[ 6 ], b21 = b[ 7 ], b22 = b[ 8 ];

        out[ 0 ] = b00 * a00 + b01 * a10 + b02 * a20;
        out[ 1 ] = b00 * a01 + b01 * a11 + b02 * a21;
        out[ 2 ] = b00 * a02 + b01 * a12 + b02 * a22;

        out[ 3 ] = b10 * a00 + b11 * a10 + b12 * a20;
        out[ 4 ] = b10 * a01 + b11 * a11 + b12 * a21;
        out[ 5 ] = b10 * a02 + b11 * a12 + b12 * a22;

        out[ 6 ] = b20 * a00 + b21 * a10 + b22 * a20;
        out[ 7 ] = b20 * a01 + b21 * a11 + b22 * a21;
        out[ 8 ] = b20 * a02 + b21 * a12 + b22 * a22;
    };

    /**
     * @param out if doesn't provided it will override input matrix
     */
    export function mat_invert( a: mat, out = a ): mat
    {
        var a00 = a[ 0 ], a01 = a[ 1 ], a02 = a[ 2 ],
            a10 = a[ 3 ], a11 = a[ 4 ], a12 = a[ 5 ],
            a20 = a[ 6 ], a21 = a[ 7 ], a22 = a[ 8 ],

            b01 = a22 * a11 - a12 * a21,
            b11 = -a22 * a10 + a12 * a20,
            b21 = a21 * a10 - a11 * a20,

            // Calculate the determinant
            det = a00 * b01 + a01 * b11 + a02 * b21;

        if ( !det )
        {
            return null;
        }
        det = 1.0 / det;

        out[ 0 ] = b01 * det;
        out[ 1 ] = ( -a22 * a01 + a02 * a21 ) * det;
        out[ 2 ] = ( a12 * a01 - a02 * a11 ) * det;
        out[ 3 ] = b11 * det;
        out[ 4 ] = ( a22 * a00 - a02 * a20 ) * det;
        out[ 5 ] = ( -a12 * a00 + a02 * a10 ) * det;
        out[ 6 ] = b21 * det;
        out[ 7 ] = ( -a21 * a00 + a01 * a20 ) * det;
        out[ 8 ] = ( a11 * a00 - a01 * a10 ) * det;
        
        return out;
    }
}

// utilites
namespace qu
{
    export function has_method<T, I>(obj: T, name: keyof I ): obj is T & I
    {
        return typeof obj[<any>name] == 'function';
    }

    export function assert(cond: boolean, msg = `Assert failed`)
    {
        if (!cond) throw new Error(msg);
    }

    export function contains<T>(arr: T[], elem: T): boolean {
        return arr.indexOf(elem) >= 0;
    }
}

// framework
namespace qf 
{
    export function unimplemented() { throw new Error("unimplemented"); }

    export abstract class component_base
    {
        public owner: actor;
        public init(): void { }
        public getworld() { return this.owner.world; }
    }

    export abstract class prmitive_component extends component_base
    {
        public pos: qm.vec = qm.v(0, 0);
        public scale: qm.vec = qm.v(1, 1);
        public rot: number = 0;
        public parent: prmitive_component;
        public bounds: qm.vec = qm.v(0, 0);

        public render_c2d(ctx: CanvasRenderingContext2D): void
        {
            ctx.save();
            ctx.translate(this.pos.x, this.pos.y);
            ctx.scale(this.scale.x, this.scale.y);
            if (this.rot != 0) {
                ctx.rotate(this.rot);
            }
            this.render_c2d_impl(ctx);
            ctx.restore();
        }

        public get_local_transform(): qm.mat
        {
            return qm.mat(this.scale.x, 0, 0, this.scale.y, this.pos.x, this.pos.y);
        }

        public get_world_transform(): qm.mat
        {
            let base = this.parent ? this.parent.get_world_transform() : qm.mat();
            qm.mat_mul(base, this.get_local_transform(), base);
            return base;
        }

        public abstract render_c2d_impl(ctx: CanvasRenderingContext2D): void;
    }

    export class rect_primitve extends prmitive_component
    {
        public fill_color: any;
        public render_c2d_impl(ctx: CanvasRenderingContext2D): void
        {
            ctx.fillStyle = this.fill_color;
            ctx.fillRect(-this.bounds.x/2, -this.bounds.y/2, this.bounds.x, this.bounds.y);
        }
    }

    export class actor
    {
        public world: world;
        public components: component_base[] = [];
        public root: prmitive_component;

        public destory() { 
            this.world.destroy_actor(this); 
        }

        public getpos(): qm.vec {
            return qm.vc(this.root.pos);
        }
        
        public getcmp<T extends component_base>(clazz: Function & { prototype: T }): T[] {
            return this.components.filter(c => c instanceof clazz) as T[];
        }
    }

    export function attach_cmp<T extends component_base>(owner: actor, cmp: T): T
    {
        cmp.owner = owner;
        owner.components.push(cmp);
        cmp.init();
        return cmp;
    }

    export function attach_prim<T extends prmitive_component>(owner: actor, prim: T, {x = 0, y = 0, width = 10, height = 10, root = false}): T
    {
        prim.pos.x = x;
        prim.pos.y = y;
        prim.bounds.x = width;
        prim.bounds.y = height;

        if (root) owner.root = prim;
        return attach_cmp(owner, prim);
    }

    export function attach_rect(owner: actor, {x = 0, y = 0, width = 10, height = 10, fill = 'red', root = false})
    {
        let r = new rect_primitve();
        r.fill_color = fill;
        return attach_prim(owner, r, {x, y, width, height, root});
    }

    export class world
    {
        public actors: actor[] = [];
        public geometry: any;
        public player: actor;
        public timer = new timer();

        public tick(delta: number)
        {
            this.timer.tick(delta);

            for (let actor of this.actors)
            {
                for (let cmp of actor.components)
                {
                    if (qu.has_method(cmp, 'tick'))
                    {
                        cmp.tick(delta);
                    }
                }
            }
        }

        public spawn_actor(): actor
        {
            let a = new actor();
            a.world = this;
            this.actors.push(a);
            return a;
        }

        public destroy_actor(actor: qf.actor): void
        {
            let i = this.actors.indexOf(actor);
            this.actors.splice(i, 1);
            actor.world = undefined;
        }

        public sweep_aabb(start: qm.vec, end: qm.vec, size: qm.vec): qm.hit_result { 
            return undefined; 
        }
    }

    export const enum timer_type
    {
        once,
        repeat
    }

    class timer_event {
        public fire_in = 0;

        constructor(
            public owner: timer,
            public type: timer_type,
            public delay: number,
            public fn: Function
        ) { 
            this.fire_in = delay;
        }

        public reset() { 
            this.fire_in = this.delay; 
        }
    }

    export class timer
    {
        public events: timer_event[] = [];

        public tick(delta: number)
        {
            for (let i = this.events.length - 1; i >= 0; --i)
            {
                let e = this.events[i];

                e.fire_in -= delta;
                if (e.fire_in <= 0) {
                    e.fn();
                    if (e.type == timer_type.once) this.events.splice(i, 1);
                    if (e.type == timer_type.repeat) e.reset();
                }
            }
        }

        public delay(delay: number, fn: Function): timer_event
        {
            let e = new timer_event(this, timer_type.once, delay, fn);
            this.events.push(e);
            return e;
        }

        public every(timespan: number, fn: Function): timer_event
        {
            let e = new timer_event(this, timer_type.repeat, timespan, fn);
            this.events.push(e);
            return e;
        }
    }
}

// render
namespace qr
{
    export function render_w(ctx: CanvasRenderingContext2D, world: qf.world)
    {
        render_a(ctx, world.actors);
    }

    export function render_a(ctx: CanvasRenderingContext2D, actors: qf.actor[])
    {
        let primitives: qf.prmitive_component[] = [];

        for (let actor of actors)
        {
            for (let cmp of actor.components)
            {
                if (cmp instanceof qf.prmitive_component) primitives.push(cmp);
            }
        }

        render_p(ctx, primitives);
    }

    export function render_p(ctx: CanvasRenderingContext2D, prmitives: qf.prmitive_component[])
    {
        for (let prim of prmitives) 
        {
            prim.render_c2d(ctx);
        }
    }
}

// system
namespace qs
{
    export namespace input
    {
        class key_state 
        { 
            constructor(
                public is_down = false, 
                public timestamp = 0
            ) { } 
        };

        export namespace keyboard
        {
            type key = 'ArrowLeft' |
                       'ArrowRight' |
                       'ArrowUp' |
                       'ArrowDown' |
                       'z' | 'x';

            const state: { [key:string]: key_state } =  { };
            export function is_down(key: key) { return state[key] ? state[key].is_down : false; }
            export function get_state(key: key | string): key_state { 
                let s = state[key];
                if (!s) state[key] = s = new key_state();
                return s;
            }

            export function just_pressed(key: key, dt_ms = 20) {
                let ks = state[key];
                if (ks && ks.is_down) {
                    return Date.now() - ks.timestamp < dt_ms;
                }
                return false;
            }

            export function on_keydown(e: KeyboardEvent) {
                let s = get_state(e.key);
                if (s.is_down) return;
                s.is_down = true;
                s.timestamp = Date.now();
            }

            export function on_keyup(e: KeyboardEvent) {
                get_state(e.key).is_down = false;
            }
        }

        export namespace mouse
        {
            export const pos = qm.v();

            export function on_move(e: MouseEvent)
            {
                pos.x = e.offsetX;
                pos.y = e.offsetY;
                e.stopPropagation();
            }
        }

        export function init(canvas: HTMLCanvasElement)
        {
            window.addEventListener('keydown', keyboard.on_keydown);
            window.addEventListener('keyup', keyboard.on_keyup);
            canvas.addEventListener('mousemove', mouse.on_move);
        }
    }
}

// collision
namespace ql
{
    export class tile_geometry
    {
        public tile_map: boolean[][] = [];

        // debug
        public d_considered: qm.vec[] = [];
        public d_hits: [qm.vec, qm.vec][] = [];

        constructor(
            public readonly width,
            public readonly height,
            public readonly tile_size = 10
        ) {
            qu.assert(width > 0 && height > 0);

            for (let i = 0; i < height; ++i)
            {
                this.tile_map[i] = [];
                for (let j = 0; j < width; ++j)
                    this.tile_map[i][j] = false;
            }
        }

        public set_blocking(x: number, y: number, blocking: boolean): void
        {
            if (x >= 0 && x < this.width && y >= 0 && y < this.height) 
            {
                this.tile_map[y][x] = blocking;
            }
            else
            {
                qu.assert(false, `${x} != [0, ${this.width}) || ${y} != [0, ${this.height})`);
            }
        }

        public is_blocking(x: number, y: number): boolean
        {
            if (x >= 0 && x < this.width && y >= 0 && y < this.height) 
            {
                return this.tile_map[y][x];
            }
            return false;
        }

        public foreach_tile_along_path(start_loc: qm.vec, end_loc: qm.vec, fn: (x: number, y: number) => boolean): void
        {
            let s = this.project_on_grid(start_loc);
            let e = this.project_on_grid(end_loc);
            let d = qm.sub(e, s);

            const dir = qm.sub(end_loc, start_loc);

            // if trace is straight horizonal or vertial line
            if (d.x == 0 || d.y == 0)
            {
                for (let y = s.y, x = s.x; ;)
                {
                    if (fn(x, y)) return;

                    if (y == e.y && x == e.x) break;
                    if (d.y == 0) x += Math.sign(d.x);
                    if (d.x == 0) y += Math.sign(d.y);
                }
                
                return;
            }

            // if trace is line
            const a = dir.y / dir.x;
            const b = start_loc.y - a * start_loc.x;
            const ts = this.tile_size;

            for (let y = s.y;;)
            {
                // x = f(y)
                let f = y => Math.floor(((y * ts - b) / a) / ts);

                let increasing = a > 0;
                let i = 0;
                let x = f(y + (increasing ? 0 : 1))
                let x_end = f(y + (increasing ? 1 : 0));

                // we have to iterate from right to left if line is decreasing on x axis
                // since tiles with bigger x are likekly to be hitted first
                if (d.x < 0)
                {
                    let t = x;
                    x = x_end;
                    x_end = t;
                }

                for (;;)
                {
                    if (fn(x, y)) return;

                    if (++i > 100) break;
                    if (x == x_end) break;

                    if (d.x > 0) ++x; 
                    else --x;
                }

                if (y != e.y) y += Math.sign(d.y);
                else break;
            }
        }
        public line_trace2(start_loc: qm.vec, end_loc: qm.vec): [qm.vec, qm.vec]
        {
            let hit_result: [qm.vec, qm.vec];

            this.foreach_tile_along_path(start_loc, end_loc, (x, y) => 
            {
                if (this.d_considered)
                    this.d_considered.push(qm.scale(qm.v(x, y), this.tile_size));

                if (this.is_blocking(x, y)) {
                    hit_result = this.line_trace_tile(start_loc, end_loc, x, y);
                    if (hit_result) {
                        this.d_hits.push(hit_result);
                        return true;
                    }
                }
            });

            return hit_result;
        }

        public line_trace_tile(start: qm.vec, end: qm.vec, x: number, y: number): qm.hit_result
        {
            const ts = this.tile_size;
            return qm.line_trace_aabb(start, end, [qm.scale(qm.v(x + 0.5, y + 0.5), ts), qm.v(ts / 2, ts / 2)]);
        }

        public sweep_aabb(start: qm.vec, end: qm.vec, size: qm.vec): qm.hit_result
        {
            let grid_size = qm.v(
                Math.max(0, Math.ceil(size.x / this.tile_size) + 1), 
                Math.max(0, Math.ceil(size.y / this.tile_size) + 1));

            let ext = qm.v(Math.floor(grid_size.x / 2), Math.floor(grid_size.y / 2));
            const ts = this.tile_size;
            const half_ts = qm.scale(qm.v(ts, ts), 0.5);
            const half_size = qm.scale(size, 0.5);

            let hit_result: qm.hit_result;

            this.foreach_tile_along_path(start, end, (x, y) => {

                let hits: qm.hit_result[] = [];

                for (let iy = y - ext.y; iy <= y + ext.y; ++iy)
                {
                    for (let ix = x - ext.x; ix <= x + ext.x; ++ix)
                    {
                        if (this.d_considered)
                            this.d_considered.push(qm.scale(qm.v(ix, iy), ts));

                        if (this.is_blocking(ix, iy))
                        {
                            let tile_center = qm.scale(qm.v(ix + 0.5, iy + 0.5), ts);
                            let hit = qm.line_trace_aabb(start, end, [tile_center, qm.add(half_size, half_ts)]);
                            if (hit)
                            {
                                this.d_hits.push(hit);
                                hits.push(hit);
                            }
                        }
                    }
                }

                let best_dist = Number.MAX_VALUE;
                for (let hit of hits)
                {
                    let [p, _] = hit;
                    let dist = qm.mag_sqr(qm.sub(p, start));
                    if (dist < best_dist)
                    {
                        best_dist = dist;
                        hit_result = hit;
                    }
                }

                if (hit_result) return true;
                else return false;
            });

            return hit_result;
        }

        public project_on_grid(point: qm.vec): qm.vec
        {
            return qm.v(Math.floor(point.x / this.tile_size), Math.floor(point.y / this.tile_size));
        }
    }
}

// components
namespace qc {
    export class character_movement extends qf.component_base {
        // setup
        // any axis
        public max_velocity = 300;
        // x axis
        public max_velocity_on_ground = 200;
        public gravity = 1000;
        public bounce_off_wall = false;

        // runtime data
        public vel = qm.v();
        public acc = qm.v();
        public last_hit: qm.hit_result;
        public on_ground = false;
        public moving_left = false;

        public trace_wall(dist = 2): qm.hit_result {
            let r = this.owner.root;
            let g = this.owner.world.geometry as ql.tile_geometry;

            let left = qm.scale(qm.left, r.bounds.x * 0.5 + dist);
            let right = qm.scale(qm.right, r.bounds.x * 0.5 + dist);

            let trace = g.line_trace2(r.pos, qm.add(r.pos, left));
            return trace ? trace : g.line_trace2(r.pos, qm.add(r.pos, right));
        }

        public trace_ground(): qm.hit_result {
            let a = this.owner;
            let r = a.root;
            let w = a.world;
            let g = w.geometry as ql.tile_geometry;

            let down = qm.scale(qm.down, r.bounds.y + 1)
            return g.line_trace2(r.pos, qm.add(r.pos, down));
        }

        public tick(delta: number) {
            let r = this.owner.root;
            let g = this.owner.world.geometry as ql.tile_geometry;

            this.vel.y += this.gravity * delta;
            this.vel = qm.add(this.vel, qm.scale(this.acc, delta));

            this.vel = qm.clamp_mag(this.vel, 0, this.max_velocity);
            this.vel.x = qm.clamp(this.vel.x, -this.max_velocity_on_ground, this.max_velocity_on_ground);

            let end = qm.add(r.pos, qm.scale(this.vel, delta));
            let trace = g.sweep_aabb(r.pos, end, r.bounds);

            if (trace) {
                let [p, n] = trace;
                this.last_hit = trace;
                r.pos.x = p.x + n.x;
                r.pos.y = p.y + n.y;

                if (n.y != 0) {
                    this.vel.y = 0;
                }
                if (n.x != 0) {
                    this.vel.x *= this.bounce_off_wall ? -qm.rnd(0.9, 1) : 0;
                }
            }
            else {
                r.pos.x = end.x;
                r.pos.y = Math.floor(end.y);
                // r.pos = end;
            }

            this.on_ground = !!this.trace_ground();
        }
    }
}

// ai
namespace qi
{
    export var paths: qm.vec[][] = [];

    export function find_path(start: qm.vec, end: qm.vec, geom: ql.tile_geometry): qm.vec[]
    {
        let s = geom.project_on_grid(start);
        let e = geom.project_on_grid(end);
        let id = (v: qm.vec) => v.y * geom.width + v.x;
        // let pos = n => [n % geom.width, Math.floor(n / geom.width)];

        let open:  qm.vec[] = [s];
        let visited: number[] = [id(s)];
        let prev:  number[] = [-1];
        // let data: {prev, dist}[] = [{prev: -1, dist: 0}];

        const offsets = [qm.v(-1, -1), /*qm.v(0, -1),*/ qm.v(1, -1),
                         qm.v(-1, 0),               qm.v(1, 0),
                         qm.v(-1, 1),  qm.v(0, 1),  qm.v(1, 1),
                                       qm.v(0, -1)];

        while (open.length) { 
            let unseen = open.shift();
            if (qm.eq(unseen, e)) break;

            for (let off of offsets)
            {
                let n_loc = qm.add(unseen, off);

                if (!geom.is_blocking(n_loc.x, n_loc.y))
                {
                    let n_id = id(n_loc);
                    if (!qu.contains(visited, n_id)) {
                        open.push(n_loc);
                        visited.push(n_id);
                        prev.push(id(unseen));
                    }
                }
            }
        }

        let id_to_loc = (id: number) => {
            return qm.scale(qm.v(id % geom.width + 0.5, Math.floor(id / geom.width) + 0.5), geom.tile_size);
        }

        let path: qm.vec[] = [id_to_loc(id(e))];
        for (let idx = visited.indexOf(id(e)); idx != -1;) {
            let tile_id = prev[idx];
            if (tile_id == -1) {
                break;
            } 

            path.unshift(id_to_loc(tile_id));
            idx = visited.indexOf(tile_id);
        }

        paths.push(path);
        return path.length > 1 ? path : undefined;
    }

    export class ai_movement extends qc.character_movement
    {
        public tick(delta: number)
        {
            if (this.on_ground)
                this.vel.x *= 0.8;
            
            super.tick(delta);
        }
    }

    export class enemy_controller extends qf.component_base
    {
        public target: qf.actor;
        public root: qf.prmitive_component;

        public init()
        {
            super.init();
            this.target = this.owner.world.player;
            this.root = this.owner.root;
        }
    }

    export class slime_ai extends enemy_controller
    {
        public mov: qc.character_movement;

        public init()
        {
            super.init();
            [this.mov] = this.owner.getcmp(qc.character_movement);
            this.mov.bounce_off_wall = true;

            this.getworld().timer.delay(qm.rnd(), _ => {
                this.getworld().timer.every(qm.rnd(2.6, 3), this.do_jump.bind(this));
            });
        }

        public do_jump(): void
        {
            let dir = qm.sign(qm.sub(this.target.getpos(), this.root.pos));
            let w = this.getworld();
            let start = this.root.pos;

            // debug
            qi.paths = [];

            let path = qi.find_path(start, this.target.root.pos, this.getworld().geometry);
            if (path) {
                dir = qm.sign(qm.sub(path[1], path[0]));
            }

            let jump = qm.scale(qm.v(300 * dir.x, -300), qm.rnd(0.8, 1));

            let hit = w.sweep_aabb(start, qm.add(start, qm.v(20 * dir.x, -20)), this.root.bounds);
            this.mov.vel = hit ? qm.mul(jump, qm.v(0.2, 1)) : jump;
        }
    }

    export function spawn_slime(world: qf.world, {x, y}): qf.actor
    {
        let a = world.spawn_actor();
        let r = qf.attach_prim(a, new qf.rect_primitve(), {x, y, root: true});
        r.fill_color = 'blue';
        qf.attach_cmp(a, new ai_movement());
        qf.attach_cmp(a, new slime_ai());
        return a;
    }
}

namespace qg
{
    class game_world extends qf.world
    {
        public geometry: ql.tile_geometry;

        public sweep_aabb(start: qm.vec, end: qm.vec, size: qm.vec): qm.hit_result
        {
            return this.geometry.sweep_aabb(start, end, size);
        }
    }

    class player_movement extends qc.character_movement
    {
        public process_input() {
            const speed = 900;

            this.acc.x = 0;
            this.acc.y = 0;
            const move_left = qs.input.keyboard.is_down('ArrowLeft');
            const move_right = qs.input.keyboard.is_down('ArrowRight');

            if (move_left) {
                this.acc.x = -speed * (this.on_ground ? 1 : 0.5);
                this.moving_left = true;
            }
            else if (move_right) {
                this.acc.x = speed * (this.on_ground ? 1 : 0.5);
                this.moving_left = false;
            }
            else if (this.on_ground)
            {
                this.vel.x *= 0.6;
            }

            if (qs.input.keyboard.is_down('ArrowUp')) {
                if (this.on_ground) {
                    this.acc.y = -speed * 100;
                }
                else if (move_left || move_right) {
                    let wall_trace = this.trace_wall(5);
                    if (wall_trace) {
                        let [p, n] = wall_trace;
                        let r = (<qf.actor>this.owner).root;
                        let dist = qm.mag(qm.sub(p, r.pos)) - r.bounds.x / 2;

                        let do_jump = false;
                        let jump_off_wall = false;

                        if (move_left) {
                            jump_off_wall = n.x < 0;
                            do_jump = dist <= (n.x > 0 ? 1 : 5);
                        }
                        if (move_right) {
                            jump_off_wall = n.x > 0;
                            do_jump = dist <= (n.x < 0 ? 1 : 5);
                        }

                        if (do_jump) {
                            this.acc.x = speed * n.x * (jump_off_wall ? 10 : 50);
                            this.acc.y = -speed * (jump_off_wall ? 60 : 200);
                        }
                    }
                }
            }
        }

        public tick(delta: number)
        {
            this.process_input();
            super.tick(delta);
        }
    }


    class projectile_movement extends qf.component_base
    {
        public acc = qm.v(0, 1000);
        public vel = qm.v();

        public tick(delta: number): void
        {
            let r = this.owner.root;
            let w = this.owner.world;

            this.vel = qm.add(this.vel, qm.scale(this.acc, delta));
            let ds = qm.scale(this.vel, delta);
            let end = qm.add(r.pos, ds);

            let hit_result = w.sweep_aabb(r.pos, end, r.bounds);
            if (hit_result)
            {
                this.vel = qm.v();
                this.acc = qm.v();
                [r.pos] = hit_result;
                setTimeout( _ => this.owner.destory(), 1000);
                this.tick = undefined;
            }
            else
            {
                r.pos = end;
                r.rot = Math.atan2(this.vel.y, this.vel.x);
            }
        }
    }

    export var g_draw_considered = false;

    class debug_draw_collisions extends qf.prmitive_component
    {
        public tiles: ql.tile_geometry;

        public init()
        {
            this.tiles = this.owner.world.geometry as ql.tile_geometry;
        }

        public render_c2d_impl(ctx: CanvasRenderingContext2D): void
        {
            const tl = this.tiles.tile_size;
            ctx.strokeStyle = 'white';
            ctx.fillStyle = 'red';

            // let to_local = this.get_world_transform();
            // qm.mat_invert(to_local);

            // const pos = qm.transform(this.query_start.root.pos, to_local)
            // const end = qm.transform(qs.input.mouse.pos, to_local);

            for (let x = 0; x < this.tiles.width; ++x)
            {
                for (let y = 0; y < this.tiles.height; ++y) 
                {
                    if (this.tiles.is_blocking(x, y))
                    {
                        ctx.strokeRect(x * tl, y * tl, tl, tl);
                        // this.tiles.line_trace_tile(pos, end, x, y);
                    }
                }
            }

            ctx.strokeStyle = 'pink';
            for (let act of this.owner.world.actors)
            {
                for (let p of act.getcmp(qf.prmitive_component)) {
                    let h = qm.scale(p.bounds, 0.5);
                    ctx.strokeRect(p.pos.x - h.x, p.pos.y - h.y, h.x * 2, h.y * 2);
                }
            }

            ctx.strokeStyle = 'green';
            for (let path of qi.paths) {
                if (path.length == 0) continue;
                ctx.beginPath();
                ctx.moveTo(path[0].x, path[0].y);
                for (let node of path) {
                    ctx.lineTo(node.x, node.y);
                }
                ctx.stroke();
            }
            // qi.paths = [];

            // ctx.beginPath();
            // ctx.moveTo(pos.x, pos.y);
            // ctx.lineTo(end.x, end.y);
            // ctx.stroke();

            let considered = this.tiles.d_considered;
            //let trace_result = this.tiles.line_trace2(pos, end);
            //this.tiles.sweep_aabb(pos, end, this.query_start.root.bounds);

            ctx.strokeStyle = 'green';
            ctx.lineWidth = 0.5;
            if (g_draw_considered)
                for (let r of considered) ctx.strokeRect(r.x, r.y, tl, tl);

            // let hits = this.tiles.d_hits;
            // for (let [p, n] of hits) ctx.fillRect(p.x - 2, p.y - 2, 4, 4);

            this.tiles.d_considered = [];
            this.tiles.d_hits = [];
        }
    }

    function spawn_projectile(world: qf.world, loc: qm.vec, dir: qm.vec): qf.actor
    {
        let a = world.spawn_actor();

        let r = qf.attach_prim(a, new qf.rect_primitve(), {root: true});
        r.pos = loc;
        r.bounds = qm.v(10, 3);
        r.fill_color = 'rgba(0, 255, 0, 0.7)';

        let p = qf.attach_cmp(a, new projectile_movement());
        p.vel = dir;

        return a;
    }

    class player_controller extends qf.component_base
    {
        private movement: player_movement;
        private z_press_start = -1;
        private last_fire_time = 0;

        public init()
        {
            this.movement = this.owner.components.find(c => c instanceof player_movement) as player_movement;
        }

        public tick(delta: number): void
        {
            let a = this.owner;
            let z_state = qs.input.keyboard.get_state('z');
            if (z_state.is_down)
            {
                this.z_press_start = qs.input.keyboard.get_state('z').timestamp;
            }

            if (this.z_press_start > 0)
            {
                const press_time = Date.now() - this.z_press_start;
                const can_fire = this.last_fire_time - this.z_press_start != 0;

                if (can_fire && (!z_state.is_down|| press_time > 1000)) {
                    this.last_fire_time = this.z_press_start;
                    this.z_press_start = 0;

                    let ampl = qm.clamp(press_time / 100, 1, 10);
                    spawn_projectile(a.world, a.root.pos,
                        qm.clamp_mag(qm.add(
                            qm.scale(this.movement.moving_left ? qm.left : qm.right, 100 * ampl),
                            qm.v(0, -200)), 0, 800));
                }
            }
        }
    }

    function parse_level(data: string, world: game_world): void
    {
        let geom = world.geometry;
        let x = 0, y = 0;
        for (let line of data.split('\n'))
        {
            x = 0;
            for (let char of line)
            {
                if (char === '#') geom.set_blocking(x, y, true);
                if (char === 's') qi.spawn_slime(world, qm.scale(qm.v(x + 0.5, y + 0.5), geom.tile_size));
                x += 1;
            }
            y += 1;
        }
    }

    let world: qf.world;
    let ctx: CanvasRenderingContext2D;

    function tick()
    {
        world.tick(1/60);
        ctx.clearRect(0, 0, 320, 320);
        qr.render_w(ctx, world);
        window.requestAnimationFrame(tick);
    }

    export function main()
    {
        let canvas: HTMLCanvasElement = document.querySelector("#canvas");
        ctx = canvas.getContext("2d");
        ctx.translate(10.5, 10.5);
        let t = new ql.tile_geometry(21, 21, 14);

        world = new game_world();
        world.geometry = t;



        let player = world.spawn_actor();

        let tdebug = new debug_draw_collisions();

        qf.attach_rect(player, {x: 100, y: 190, width: 12, height: 12, fill: 'rgba(255, 0, 0, .5)', root: true});
        qf.attach_cmp(player, new player_movement());
        qf.attach_prim(player, tdebug, {x: 0, y: 0});
        qf.attach_cmp(player, new player_controller());

        world.player = player;

        parse_level(
`####################
#                  #
#                  #
#              s   #
#   ####      ###  #
#      #      #    #
#      #      #    #
#      #      #    #
#      #      #    #
#      #      #    #
#      #      #    #
#                  #
#                  #
#       s          #
#       ###        #
#                  #
#                  #
#           ####   #
#           #      #
#           #      #
####################`
                    , world)

        qs.input.init(canvas);
        window.requestAnimationFrame(tick);
    }
}

qg.main();