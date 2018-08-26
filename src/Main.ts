// math
namespace qm
{
    export class vec 
    { 
        constructor(
            public x:number = 0,
            public y: number = 0
        ) { }
    }
    
    export const up = new vec(0, 1);
    export const down = new vec(0, -1);
    export const left = new vec(1, 0);
    export const right = new vec(-1, 0);

    export function v(x: number = 0, y: number = 0) { return new vec(x, y); }
    export function add(a: vec, b: vec) { return v(a.x + b.x, a.y + b.y); }
    export function sub(a: vec, b: vec) { return v(a.x - b.x, a.y - b.y); }
    export function scale(a: vec, s: number) { return v(a.x * s, a.y * s); }
    export function cross(a: vec, b: vec) { return a.x * b.y - a.y * b.x; }
    export function sqr_dist(a: vec) { return a.x * a.x + a.y * a.y; }

    export function aabb(a: vec, b: vec): [vec, vec]
    {
        return [v(Math.min(a.x, b.x), Math.min(a.y, b.y)), v(Math.max(a.x, b.x), Math.max(a.y, b.y))];
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

    export function swap<T>(a: T, b: T): void
    {
        let t = a;
        a = b;
        b = t; 
    }
}

// core
namespace qc 
{
    export function unimplemented() { throw new Error("unimplemented"); }

    export const enum component_id
    {
        invalid = -1,
        rect_primitive,
        tickable,
    }

    export abstract class component_base
    {
        public owner: any;
        public abstract id(): number;
    }

    export abstract class prmitive_component extends component_base
    {
        public pos: qm.vec = qm.v();
        public scale: qm.vec = qm.v();
        public rot: number = 0;
        public parent: prmitive_component;
        public bounds: qm.vec = qm.v();
        public abstract render_c2d(ctx: CanvasRenderingContext2D): void;
    }

    export class rect_primitve extends prmitive_component
    {
        public fill_color: any;
        public id(): number { return component_id.rect_primitive; }
        public render_c2d(ctx: CanvasRenderingContext2D): void
        {
            ctx.fillStyle = this.fill_color;
            ctx.fillRect(this.pos.x, this.pos.y, this.bounds.x, this.bounds.y);
        }
    }

    export class actor
    {
        public world: any;
        public components: component_base[] = [];
        public root: prmitive_component;
    }

    export function attach_cmp<T extends component_base>(owner: actor, cmp?: T): T
    {
        cmp.owner = owner;
        owner.components.push(cmp);
        return cmp;
    }

    export function attach_prim(owner: actor, prim: prmitive_component, {x = 0, y = 0, width = 10, height = 10, root = false})
    {
        attach_cmp(owner, prim);
        prim.pos.x = x;
        prim.pos.y = y;
        prim.bounds.x = width;
        prim.bounds.y = height;

        if (root) owner.root = prim;
        return prim;
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

        public tick(delta: number)
        {
            for (let actor of this.actors)
            {
                for (let cmp of actor.components)
                {
                    if (qu.has_method(cmp, 'tick'))
                    {
                        cmp.tick();
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
    }
}

// render
namespace qr
{
    export function render_w(ctx: CanvasRenderingContext2D, world: qc.world)
    {
        render_a(ctx, world.actors);
    }

    export function render_a(ctx: CanvasRenderingContext2D, actors: qc.actor[])
    {
        let primitives: qc.prmitive_component[] = [];

        for (let actor of actors)
        {
            for (let cmp of actor.components)
            {
                if (cmp instanceof qc.prmitive_component) primitives.push(cmp);
            }
        }

        render_p(ctx, primitives);
    }

    export function render_p(ctx: CanvasRenderingContext2D, prmitives: qc.prmitive_component[])
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
                       'ArrowDown';

            const state: { [key:string]: key_state } =  { };
            export function is_down(key: key) { return state[key] ? state[key].is_down : false; }

            export function on_keydown(e: KeyboardEvent)
            {
                if (!state[e.key]) state[e.key] = new key_state();
                if (state[e.key].is_down) return;
                state[e.key].is_down = true;
                state[e.key].timestamp = Date.now();
                console.log(state);
            }

            export function on_keyup(e: KeyboardEvent)
            {
                if (!state[e.key]) state[e.key] = new key_state();
                state[e.key].is_down = false;
                console.log(state);
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
    export class tile_world
    {
        public tile_map: boolean[][] = [];
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

        public line_trace2(start_loc: qm.vec, end_loc: qm.vec, considered?: qm.vec[]): [qm.vec, qm.vec]
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
                    if (considered)
                        considered.push(qm.scale(qm.v(x, y), this.tile_size));

                    if (this.is_blocking(x, y)) 
                    {
                        let hit_result = this.line_trace_tile(start_loc, end_loc, x, y);
                        if (hit_result) 
                        {
                            return hit_result;
                        }
                    }
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
                    if (considered)
                        considered.push(qm.scale(qm.v(x, y), this.tile_size));

                    if (this.is_blocking(x, y)) 
                    {
                        let hit_result = this.line_trace_tile(start_loc, end_loc, x, y);
                        if (hit_result) 
                        {
                            return hit_result;
                        }
                    }

                    if (++i > 100) break;
                    if (x == x_end) break;

                    if (d.x > 0) ++x; 
                    else --x;
                }

                if (y != e.y) y += Math.sign(d.y);
                else break;
            }

        }

        public line_trace_tile(start: qm.vec, end: qm.vec, x: number, y: number): [qm.vec, qm.vec]
        {
            let get_vertex = (i: number) => {
                const hx = [0, 1, 1, 0];
                const hy = [0, 0, 1, 1];
                return qm.scale(qm.v(x + hx[i % 4], y + hy[i % 4]), this.tile_size);
            };

            const trace = qm.sub(end, start);
            let hits: qm.vec[] = new Array(4);

            for (let i = 0; i < 4; ++i)
            {
                let v1 = get_vertex(i), 
                    v2 = get_vertex(i + 1);
                let edge = qm.sub(v2, v1);

                // if trace start point is above edge
                if (qm.cross(edge, qm.sub(start, v1)) < 0 && 
                // and end trace is below line
                    qm.cross(edge, qm.sub(end, v1)) > 0)
                {
                    // trace is vertical line
                    if (trace.x == 0)
                    {
                        // we can only colide against horizontal line
                        if (i == 0 || i == 2)
                        {
                            if (start.x > v1.x && start.x < v2.x)
                            {
                                hits[i] = qm.v(start.x, v1.y);
                            }
                        }
                        continue;
                    }

                    const a = trace.y / trace.x;
                    const b = start.y - a * start.x;

                    // horizontal line
                    if ((i == 0 || i == 2) && a != 0)
                    {
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
                    let dist = qm.sqr_dist(qm.sub(hits[i], start));
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

        private project_on_grid(point: qm.vec): qm.vec
        {
            return qm.v(Math.floor(point.x / this.tile_size), Math.floor(point.y / this.tile_size));
        }
    }
}

namespace qg
{
    class mov extends qc.component_base
    {
        public id(): number { return 0; }
        public tick(delta: number)
        {
            let a = this.owner as qc.actor;
            let r = a.root;
            // r.pos.x += 1;
            // r.pos.x %= 100;

            if (qs.input.keyboard.is_down('ArrowLeft'))
            {
                r.pos.x -= 1;
            }
            else if (qs.input.keyboard.is_down('ArrowRight'))
            {
                r.pos.x += 1;
            }
            else if (qs.input.keyboard.is_down('ArrowDown'))
            {
                r.pos.y += 1;
            }
            else if (qs.input.keyboard.is_down('ArrowUp'))
            {
                r.pos.y -= 1;
            }
        }
    }

    class debug_draw_tile extends qc.prmitive_component
    {
        public tiles: ql.tile_world;
        public query_start: qc.actor;

        public id() { return 0; }
        public render_c2d(ctx: CanvasRenderingContext2D): void
        {
            const tl = this.tiles.tile_size;
            ctx.strokeStyle = 'white';
            ctx.fillStyle = 'red';

            const pos = this.query_start.root.pos;
            const end = qs.input.mouse.pos;

            for (let x = 0; x < this.tiles.width; ++x) 
            {
                for (let y = 0; y < this.tiles.height; ++y) 
                {
                    if (this.tiles.is_blocking(x, y))
                    {
                        ctx.strokeRect(x * tl, y * tl, tl, tl);

                        let r = this.tiles.line_trace_tile(pos, end, x, y);
                        if (r)
                        {
                            let [p, n] = r;
                            ctx.fillRect(p.x, p.y, 4, 4);
                        }
                    }
                }
            }

            ctx.beginPath();
            ctx.moveTo(pos.x, pos.y);
            ctx.lineTo(end.x, end.y);
            ctx.stroke();

            let considered: qm.vec[] = [];
            let trace_result = this.tiles.line_trace2(this.query_start.root.pos, qs.input.mouse.pos, considered);

            ctx.strokeStyle = 'green';
            for (let r of considered) ctx.strokeRect(r.x, r.y, tl, tl);

            if (trace_result)
            {
                ctx.fillStyle = 'green';
                let [p, n] = trace_result;
                ctx.fillRect(p.x - 4, p.y - 4, 8, 8);
            }
        }
    }

    let world: qc.world;
    let ctx: CanvasRenderingContext2D;

    function tick()
    {
        world.tick(30);
        ctx.clearRect(0, 0, 320, 320);
        qr.render_w(ctx, world);
        window.requestAnimationFrame(tick);
    }

    export function main()
    {
        let canvas: HTMLCanvasElement = document.querySelector("#canvas");
        ctx = canvas.getContext("2d");
        ctx.translate(0.5, 0.5);
        let t = new ql.tile_world(20, 20, 14);

        for (let i = 0; i < 20; ++i)
            t.set_blocking(i, 19, true);
        for (let i = 0; i < 20; ++i)
            t.set_blocking(i, 0, true);
        for (let i = 7; i < 17; ++i)
            t.set_blocking(i, 10, true);
        for (let i = 0; i < 20; ++i)
            t.set_blocking(0, i, true);
        for (let i = 0; i < 20; ++i)
            t.set_blocking(19, i, true);

        world = new qc.world();
        let p1 = world.spawn_actor();

        let tdebug = new debug_draw_tile();
        tdebug.tiles = t;
        tdebug.query_start = p1;

        qc.attach_rect(p1, {x: 100, y: 170, root: true});
        qc.attach_cmp(p1, new mov());
        qc.attach_prim(p1, tdebug, {});

        qs.input.init(canvas);
        window.requestAnimationFrame(tick);
    }
}

qg.main();