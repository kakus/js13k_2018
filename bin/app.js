// math
var qm;
(function (qm) {
    class vec {
        constructor(x = 0, y = 0) {
            this.x = x;
            this.y = y;
        }
    }
    qm.vec = vec;
    qm.up = new vec(0, 1);
    qm.down = new vec(0, -1);
    qm.left = new vec(1, 0);
    qm.right = new vec(-1, 0);
    function v(x = 0, y = 0) { return new vec(x, y); }
    qm.v = v;
    function add(a, b) { return v(a.x + b.x, a.y + b.y); }
    qm.add = add;
    function sub(a, b) { return v(a.x - b.x, a.y - b.y); }
    qm.sub = sub;
    function scale(a, s) { return v(a.x * s, a.y * s); }
    qm.scale = scale;
    function cross(a, b) { return a.x * b.y - a.y * b.x; }
    qm.cross = cross;
    function sqr_dist(a) { return a.x * a.x + a.y * a.y; }
    qm.sqr_dist = sqr_dist;
    function aabb(a, b) {
        return [v(Math.min(a.x, b.x), Math.min(a.y, b.y)), v(Math.max(a.x, b.x), Math.max(a.y, b.y))];
    }
    qm.aabb = aabb;
})(qm || (qm = {}));
// utilites
var qu;
(function (qu) {
    function has_method(obj, name) {
        return typeof obj[name] == 'function';
    }
    qu.has_method = has_method;
    function assert(cond, msg = `Assert failed`) {
        if (!cond)
            throw new Error(msg);
    }
    qu.assert = assert;
    function swap(a, b) {
        let t = a;
        a = b;
        b = t;
    }
    qu.swap = swap;
})(qu || (qu = {}));
// core
var qc;
(function (qc) {
    function unimplemented() { throw new Error("unimplemented"); }
    qc.unimplemented = unimplemented;
    class component_base {
    }
    qc.component_base = component_base;
    class prmitive_component extends component_base {
        constructor() {
            super(...arguments);
            this.pos = qm.v();
            this.scale = qm.v();
            this.rot = 0;
            this.bounds = qm.v();
        }
    }
    qc.prmitive_component = prmitive_component;
    class rect_primitve extends prmitive_component {
        id() { return 0 /* rect_primitive */; }
        render_c2d(ctx) {
            ctx.fillStyle = this.fill_color;
            ctx.fillRect(this.pos.x, this.pos.y, this.bounds.x, this.bounds.y);
        }
    }
    qc.rect_primitve = rect_primitve;
    class actor {
        constructor() {
            this.components = [];
        }
    }
    qc.actor = actor;
    function attach_cmp(owner, cmp) {
        cmp.owner = owner;
        owner.components.push(cmp);
        return cmp;
    }
    qc.attach_cmp = attach_cmp;
    function attach_prim(owner, prim, { x = 0, y = 0, width = 10, height = 10, root = false }) {
        attach_cmp(owner, prim);
        prim.pos.x = x;
        prim.pos.y = y;
        prim.bounds.x = width;
        prim.bounds.y = height;
        if (root)
            owner.root = prim;
        return prim;
    }
    qc.attach_prim = attach_prim;
    function attach_rect(owner, { x = 0, y = 0, width = 10, height = 10, fill = 'red', root = false }) {
        let r = new rect_primitve();
        r.fill_color = fill;
        return attach_prim(owner, r, { x, y, width, height, root });
    }
    qc.attach_rect = attach_rect;
    class world {
        constructor() {
            this.actors = [];
        }
        tick(delta) {
            for (let actor of this.actors) {
                for (let cmp of actor.components) {
                    if (qu.has_method(cmp, 'tick')) {
                        cmp.tick();
                    }
                }
            }
        }
        spawn_actor() {
            let a = new actor();
            a.world = this;
            this.actors.push(a);
            return a;
        }
    }
    qc.world = world;
})(qc || (qc = {}));
// render
var qr;
(function (qr) {
    function render_w(ctx, world) {
        render_a(ctx, world.actors);
    }
    qr.render_w = render_w;
    function render_a(ctx, actors) {
        let primitives = [];
        for (let actor of actors) {
            for (let cmp of actor.components) {
                if (cmp instanceof qc.prmitive_component)
                    primitives.push(cmp);
            }
        }
        render_p(ctx, primitives);
    }
    qr.render_a = render_a;
    function render_p(ctx, prmitives) {
        for (let prim of prmitives) {
            prim.render_c2d(ctx);
        }
    }
    qr.render_p = render_p;
})(qr || (qr = {}));
// system
var qs;
(function (qs) {
    let input;
    (function (input) {
        class key_state {
            constructor(is_down = false, timestamp = 0) {
                this.is_down = is_down;
                this.timestamp = timestamp;
            }
        }
        ;
        let keyboard;
        (function (keyboard) {
            const state = {};
            function is_down(key) { return state[key] ? state[key].is_down : false; }
            keyboard.is_down = is_down;
            function on_keydown(e) {
                if (!state[e.key])
                    state[e.key] = new key_state();
                if (state[e.key].is_down)
                    return;
                state[e.key].is_down = true;
                state[e.key].timestamp = Date.now();
                console.log(state);
            }
            keyboard.on_keydown = on_keydown;
            function on_keyup(e) {
                if (!state[e.key])
                    state[e.key] = new key_state();
                state[e.key].is_down = false;
                console.log(state);
            }
            keyboard.on_keyup = on_keyup;
        })(keyboard = input.keyboard || (input.keyboard = {}));
        let mouse;
        (function (mouse) {
            mouse.pos = qm.v();
            function on_move(e) {
                mouse.pos.x = e.offsetX;
                mouse.pos.y = e.offsetY;
                e.stopPropagation();
            }
            mouse.on_move = on_move;
        })(mouse = input.mouse || (input.mouse = {}));
        function init(canvas) {
            window.addEventListener('keydown', keyboard.on_keydown);
            window.addEventListener('keyup', keyboard.on_keyup);
            canvas.addEventListener('mousemove', mouse.on_move);
        }
        input.init = init;
    })(input = qs.input || (qs.input = {}));
})(qs || (qs = {}));
// collision
var ql;
(function (ql) {
    class tile_world {
        constructor(width, height, tile_size = 10) {
            this.width = width;
            this.height = height;
            this.tile_size = tile_size;
            this.tile_map = [];
            qu.assert(width > 0 && height > 0);
            for (let i = 0; i < height; ++i) {
                this.tile_map[i] = [];
                for (let j = 0; j < width; ++j)
                    this.tile_map[i][j] = false;
            }
        }
        set_blocking(x, y, blocking) {
            if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
                this.tile_map[y][x] = blocking;
            }
            else {
                qu.assert(false, `${x} != [0, ${this.width}) || ${y} != [0, ${this.height})`);
            }
        }
        is_blocking(x, y) {
            if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
                return this.tile_map[y][x];
            }
            return false;
        }
        line_trace2(start_loc, end_loc, considered) {
            let s = this.project_on_grid(start_loc);
            let e = this.project_on_grid(end_loc);
            let d = qm.sub(e, s);
            const dir = qm.sub(end_loc, start_loc);
            // if trace is straight horizonal or vertial line
            if (d.x == 0 || d.y == 0) {
                for (let y = s.y, x = s.x;;) {
                    if (considered)
                        considered.push(qm.scale(qm.v(x, y), this.tile_size));
                    if (this.is_blocking(x, y)) {
                        let hit_result = this.line_trace_tile(start_loc, end_loc, x, y);
                        if (hit_result) {
                            return hit_result;
                        }
                    }
                    if (y == e.y && x == e.x)
                        break;
                    if (d.y == 0)
                        x += Math.sign(d.x);
                    if (d.x == 0)
                        y += Math.sign(d.y);
                }
                return;
            }
            // if trace is line
            const a = dir.y / dir.x;
            const b = start_loc.y - a * start_loc.x;
            const ts = this.tile_size;
            for (let y = s.y;;) {
                // x = f(y)
                let f = y => Math.floor(((y * ts - b) / a) / ts);
                let increasing = a > 0;
                let i = 0;
                let x = f(y + (increasing ? 0 : 1));
                let x_end = f(y + (increasing ? 1 : 0));
                // we have to iterate from right to left if line is decreasing on x axis
                // since tiles with bigger x are likekly to be hitted first
                if (d.x < 0) {
                    let t = x;
                    x = x_end;
                    x_end = t;
                }
                for (;;) {
                    if (considered)
                        considered.push(qm.scale(qm.v(x, y), this.tile_size));
                    if (this.is_blocking(x, y)) {
                        let hit_result = this.line_trace_tile(start_loc, end_loc, x, y);
                        if (hit_result) {
                            return hit_result;
                        }
                    }
                    if (++i > 100)
                        break;
                    if (x == x_end)
                        break;
                    if (d.x > 0)
                        ++x;
                    else
                        --x;
                }
                if (y != e.y)
                    y += Math.sign(d.y);
                else
                    break;
            }
        }
        line_trace_tile(start, end, x, y) {
            let get_vertex = (i) => {
                const hx = [0, 1, 1, 0];
                const hy = [0, 0, 1, 1];
                return qm.scale(qm.v(x + hx[i % 4], y + hy[i % 4]), this.tile_size);
            };
            const trace = qm.sub(end, start);
            let hits = new Array(4);
            for (let i = 0; i < 4; ++i) {
                let v1 = get_vertex(i), v2 = get_vertex(i + 1);
                let edge = qm.sub(v2, v1);
                // if trace start point is above edge
                if (qm.cross(edge, qm.sub(start, v1)) < 0 &&
                    // and end trace is below line
                    qm.cross(edge, qm.sub(end, v1)) > 0) {
                    // trace is vertical line
                    if (trace.x == 0) {
                        // we can only colide against horizontal line
                        if (i == 0 || i == 2) {
                            if (start.x > v1.x && start.x < v2.x) {
                                hits[i] = qm.v(start.x, v1.y);
                            }
                        }
                        continue;
                    }
                    const a = trace.y / trace.x;
                    const b = start.y - a * start.x;
                    // horizontal line
                    if ((i == 0 || i == 2) && a != 0) {
                        let y = v1.y;
                        let x = (y - b) / a;
                        if (x >= Math.min(v1.x, v2.x) && x <= Math.max(v1.x, v2.x)) {
                            hits[i] = qm.v(x, y);
                        }
                    }
                    // vertical line
                    else {
                        let x = v1.x;
                        let y = a * x + b;
                        if (y >= Math.min(v1.y, v2.y) && y <= Math.max(v1.y, v2.y)) {
                            hits[i] = qm.v(x, y);
                        }
                    }
                }
            }
            let best_dist = Number.MAX_VALUE;
            let best_index = -1;
            for (let i = 0; i < 4; ++i) {
                if (hits[i]) {
                    let dist = qm.sqr_dist(qm.sub(hits[i], start));
                    if (dist < best_dist) {
                        best_dist = dist;
                        best_index = i;
                    }
                }
            }
            if (best_index != -1) {
                const normal = [qm.up, qm.right, qm.down, qm.left];
                return [hits[best_index], normal[best_index]];
            }
        }
        project_on_grid(point) {
            return qm.v(Math.floor(point.x / this.tile_size), Math.floor(point.y / this.tile_size));
        }
    }
    ql.tile_world = tile_world;
})(ql || (ql = {}));
var qg;
(function (qg) {
    class mov extends qc.component_base {
        id() { return 0; }
        tick(delta) {
            let a = this.owner;
            let r = a.root;
            // r.pos.x += 1;
            // r.pos.x %= 100;
            if (qs.input.keyboard.is_down('ArrowLeft')) {
                r.pos.x -= 1;
            }
            else if (qs.input.keyboard.is_down('ArrowRight')) {
                r.pos.x += 1;
            }
            else if (qs.input.keyboard.is_down('ArrowDown')) {
                r.pos.y += 1;
            }
            else if (qs.input.keyboard.is_down('ArrowUp')) {
                r.pos.y -= 1;
            }
        }
    }
    class debug_draw_tile extends qc.prmitive_component {
        id() { return 0; }
        render_c2d(ctx) {
            const tl = this.tiles.tile_size;
            ctx.strokeStyle = 'white';
            ctx.fillStyle = 'red';
            const pos = this.query_start.root.pos;
            const end = qs.input.mouse.pos;
            for (let x = 0; x < this.tiles.width; ++x) {
                for (let y = 0; y < this.tiles.height; ++y) {
                    if (this.tiles.is_blocking(x, y)) {
                        ctx.strokeRect(x * tl, y * tl, tl, tl);
                        let r = this.tiles.line_trace_tile(pos, end, x, y);
                        if (r) {
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
            let considered = [];
            let trace_result = this.tiles.line_trace2(this.query_start.root.pos, qs.input.mouse.pos, considered);
            ctx.strokeStyle = 'green';
            for (let r of considered)
                ctx.strokeRect(r.x, r.y, tl, tl);
            if (trace_result) {
                ctx.fillStyle = 'green';
                let [p, n] = trace_result;
                ctx.fillRect(p.x - 4, p.y - 4, 8, 8);
            }
        }
    }
    let world;
    let ctx;
    function tick() {
        world.tick(30);
        ctx.clearRect(0, 0, 320, 320);
        qr.render_w(ctx, world);
        window.requestAnimationFrame(tick);
    }
    function main() {
        let canvas = document.querySelector("#canvas");
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
        qc.attach_rect(p1, { x: 100, y: 170, root: true });
        qc.attach_cmp(p1, new mov());
        qc.attach_prim(p1, tdebug, {});
        qs.input.init(canvas);
        window.requestAnimationFrame(tick);
    }
    qg.main = main;
})(qg || (qg = {}));
qg.main();
