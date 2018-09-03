// math
var qm;
(function (qm) {
    class vec {
        constructor(x, y) {
            this.x = x;
            this.y = y;
        }
    }
    qm.vec = vec;
    class cvec {
        constructor(x, y) {
            this.x = x;
            this.y = y;
        }
    }
    qm.cvec = cvec;
    qm.one = new cvec(1, 1);
    qm.zero = new cvec(0, 0);
    qm.up = new cvec(0, -1);
    qm.down = new cvec(0, 1);
    qm.left = new cvec(-1, 0);
    qm.right = new cvec(1, 0);
    qm.top_left = new cvec(-1, -1);
    qm.top_right = new cvec(1, -1);
    qm.bottom_left = new cvec(-1, 1);
    qm.bottom_right = new cvec(1, 1);
    function v(x = 0, y = 0) { return new vec(x, y); }
    qm.v = v;
    function vc(a) { return new vec(a.x, a.y); }
    qm.vc = vc;
    function eq(a, b) { return a.x == b.x && a.y == b.y; }
    qm.eq = eq;
    function add(a, b) { return v(a.x + b.x, a.y + b.y); }
    qm.add = add;
    function sub(a, b) { return v(a.x - b.x, a.y - b.y); }
    qm.sub = sub;
    function mul(a, b) { return v(a.x * b.x, a.y * b.y); }
    qm.mul = mul;
    function dot(a, b) { return a.x * b.x + a.y * b.y; }
    qm.dot = dot;
    function scale(a, s) { return v(a.x * s, a.y * s); }
    qm.scale = scale;
    function cross(a, b) { return a.x * b.y - a.y * b.x; }
    qm.cross = cross;
    function sign(a) { return v(Math.sign(a.x), Math.sign(a.y)); }
    qm.sign = sign;
    function mag_sqr(a) { return a.x * a.x + a.y * a.y; }
    qm.mag_sqr = mag_sqr;
    function mag(a) { return Math.sqrt(mag_sqr(a)); }
    qm.mag = mag;
    function unit(a) { let m = mag(a); return scale(a, 1 / m); }
    qm.unit = unit;
    function clamp_mag(a, min, max) {
        let m = mag(a);
        return m < min ? scale(unit(a), min) :
            m > max ? scale(unit(a), max) :
                a;
    }
    qm.clamp_mag = clamp_mag;
    function manhattan_dist(a) { return Math.abs(a.x) + Math.abs(a.y); }
    qm.manhattan_dist = manhattan_dist;
    // general math
    function clamp(x, min, max) { return Math.max(Math.min(x, max), min); }
    qm.clamp = clamp;
    function eq_eps(a, b, eps = 0.01) { return Math.abs(a - b) < eps; }
    qm.eq_eps = eq_eps;
    function rnd(min = 0, max = 1) { return min + (max - min) * Math.random(); }
    qm.rnd = rnd;
    function rnd_select(...elements) { return elements[qm.rnd(0, elements.length) | 0]; }
    qm.rnd_select = rnd_select;
    ;
    // a, b - any two points
    // return [center, half_size]
    function make_aabb(a, b) {
        let tl = v(Math.min(a.x, b.x), Math.min(a.y, b.y));
        let br = v(Math.max(a.x, b.x), Math.max(a.y, b.y));
        return [qm.scale(qm.add(tl, br), 0.5), qm.scale(qm.sub(br, tl), 0.5)];
    }
    qm.make_aabb = make_aabb;
    function overlap_point(a, [c, ext]) {
        let d = qm.sub(a, c);
        return Math.abs(d.x) < ext.x && Math.abs(d.y) < ext.y;
    }
    qm.overlap_point = overlap_point;
    function overlap_aabb([ac, aext], [bc, bext]) {
        return qm.overlap_point(ac, [bc, qm.add(aext, bext)]);
    }
    qm.overlap_aabb = overlap_aabb;
    function line_trace_aabb(start, end, aabb) {
        let get_vertex = (i) => {
            let [center, ext] = aabb;
            const hx = [-ext.x, ext.x, ext.x, -ext.x];
            const hy = [-ext.y, -ext.y, ext.y, ext.y];
            return qm.v(center.x + hx[i % 4], center.y + hy[i % 4]);
        };
        const trace = qm.sub(end, start);
        let hits = new Array(4);
        for (let i = 0; i < 4; ++i) {
            let v1 = get_vertex(i), v2 = get_vertex(i + 1);
            let edge = qm.sub(v2, v1);
            // if trace start point is above edge
            if (qm.cross(edge, qm.sub(start, v1)) <= 0 &&
                // and end trace is below line
                qm.cross(edge, qm.sub(end, v1)) >= 0) {
                // trace is vertical line
                if (trace.x == 0) {
                    // we can only colide against horizontal line
                    if (i == 0 || i == 2) {
                        if (start.x > Math.min(v1.x, v2.x) && start.x < Math.max(v1.x, v2.x)) {
                            hits[i] = qm.v(start.x, v1.y);
                        }
                    }
                    continue;
                }
                const a = trace.y / trace.x;
                const b = start.y - a * start.x;
                // horizontal line
                if (i == 0 || i == 2) {
                    // we can't hit horizontal line with horizontal trace
                    if (a == 0)
                        continue;
                    let y = v1.y;
                    let x = (y - b) / a;
                    if (x > Math.min(v1.x, v2.x) && x < Math.max(v1.x, v2.x)) {
                        hits[i] = qm.v(x, y);
                    }
                }
                // vertical line
                else {
                    let x = v1.x;
                    let y = a * x + b;
                    if (y > Math.min(v1.y, v2.y) && y < Math.max(v1.y, v2.y)) {
                        hits[i] = qm.v(x, y);
                    }
                }
            }
        }
        let best_dist = Number.MAX_VALUE;
        let best_index = -1;
        for (let i = 0; i < 4; ++i) {
            if (hits[i]) {
                let dist = qm.mag_sqr(qm.sub(hits[i], start));
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
    qm.line_trace_aabb = line_trace_aabb;
    function mat(m00 = 1, m10 = 0, m01 = 0, m11 = 1, tx = 0, ty = 0) {
        return new Float32Array([m00, m10, 0, m01, m11, 0, tx, ty, 1]);
    }
    qm.mat = mat;
    function transform(v, m) {
        return qm.v(v.x * m[0] + v.y * m[3] + m[6], v.x * m[1] + v.y * m[4] + m[7]);
    }
    qm.transform = transform;
    function mat_mul(a, b, out) {
        var a00 = a[0], a01 = a[1], a02 = a[2], a10 = a[3], a11 = a[4], a12 = a[5], a20 = a[6], a21 = a[7], a22 = a[8], b00 = b[0], b01 = b[1], b02 = b[2], b10 = b[3], b11 = b[4], b12 = b[5], b20 = b[6], b21 = b[7], b22 = b[8];
        out[0] = b00 * a00 + b01 * a10 + b02 * a20;
        out[1] = b00 * a01 + b01 * a11 + b02 * a21;
        out[2] = b00 * a02 + b01 * a12 + b02 * a22;
        out[3] = b10 * a00 + b11 * a10 + b12 * a20;
        out[4] = b10 * a01 + b11 * a11 + b12 * a21;
        out[5] = b10 * a02 + b11 * a12 + b12 * a22;
        out[6] = b20 * a00 + b21 * a10 + b22 * a20;
        out[7] = b20 * a01 + b21 * a11 + b22 * a21;
        out[8] = b20 * a02 + b21 * a12 + b22 * a22;
    }
    qm.mat_mul = mat_mul;
    ;
    /**
     * @param out if doesn't provided it will override input matrix
     */
    function mat_invert(a, out = a) {
        var a00 = a[0], a01 = a[1], a02 = a[2], a10 = a[3], a11 = a[4], a12 = a[5], a20 = a[6], a21 = a[7], a22 = a[8], b01 = a22 * a11 - a12 * a21, b11 = -a22 * a10 + a12 * a20, b21 = a21 * a10 - a11 * a20, 
        // Calculate the determinant
        det = a00 * b01 + a01 * b11 + a02 * b21;
        if (!det) {
            return null;
        }
        det = 1.0 / det;
        out[0] = b01 * det;
        out[1] = (-a22 * a01 + a02 * a21) * det;
        out[2] = (a12 * a01 - a02 * a11) * det;
        out[3] = b11 * det;
        out[4] = (a22 * a00 - a02 * a20) * det;
        out[5] = (-a12 * a00 + a02 * a10) * det;
        out[6] = b21 * det;
        out[7] = (-a21 * a00 + a01 * a20) * det;
        out[8] = (a11 * a00 - a01 * a10) * det;
        return out;
    }
    qm.mat_invert = mat_invert;
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
    function contains(arr, elem) {
        return arr.indexOf(elem) >= 0;
    }
    qu.contains = contains;
    function upper_bound(arr, cmp) {
        if (arr.length == 0) {
            return undefined;
        }
        let selected = 0;
        for (let i = 1; i < arr.length; ++i) {
            if (cmp(arr[selected], arr[i])) {
                selected = i;
            }
        }
        return arr[selected];
    }
    qu.upper_bound = upper_bound;
})(qu || (qu = {}));
// framework
var qf;
(function (qf) {
    function unimplemented() { throw new Error("unimplemented"); }
    qf.unimplemented = unimplemented;
    class multicast_delegate {
        constructor() {
            this.delegates = [];
        }
        broadcast(...args) {
            for (let i = this.delegates.length - 1; i >= 0; --i) {
                let [d, owner] = this.delegates[i];
                if (owner.is_valid()) {
                    d.apply(owner, args);
                }
                else {
                    this.delegates.splice(i, 1);
                }
            }
        }
        bind(fn, owner) {
            if (owner.is_valid()) {
                this.delegates.push([fn, owner]);
            }
        }
    }
    qf.multicast_delegate = multicast_delegate;
    class hit_result {
        constructor(pos = qm.zero, normal = qm.up, actor = undefined) {
            this.pos = pos;
            this.normal = normal;
            this.actor = actor;
        }
    }
    qf.hit_result = hit_result;
    class component_base {
        constructor() {
            this.name = '';
        }
        begin_play() { }
        get_world() { return this.owner.world; }
        get_timer() { return this.owner.world.timer; }
        is_valid() { return this.owner && this.owner.is_valid(); }
    }
    qf.component_base = component_base;
    class scene_component extends component_base {
        constructor() {
            super(...arguments);
            this.pos = qm.v(0, 0);
            this.scale = qm.v(1, 1);
            this.rot = 0;
            this.bounds = qm.v(0, 0);
            this.visible = true;
            this.collision_mask = 0 /* none */;
        }
        render_c2d(ctx) {
            let pos = this.pos;
            let scale = this.scale;
            if (this.parent) {
                let t = this.get_world_transform();
                pos = qm.transform(qm.zero, t);
                scale = qm.sub(qm.transform(qm.one, t), pos);
            }
            ctx.save();
            ctx.translate(pos.x, pos.y);
            ctx.scale(scale.x, scale.y);
            if (this.rot != 0) {
                ctx.rotate(this.rot);
            }
            this.render_c2d_impl(ctx);
            ctx.restore();
        }
        get_pos() {
            if (this.parent) {
                return qm.transform(qm.zero, this.get_world_transform());
            }
            return qm.vc(this.pos);
        }
        get_aabb(ext = qm.zero) {
            return [qm.vc(this.pos), qm.add(qm.scale(qm.mul(this.bounds, this.scale), 0.5), ext)];
        }
        get_local_transform() {
            return qm.mat(this.scale.x, 0, 0, this.scale.y, this.pos.x, this.pos.y);
        }
        get_world_transform() {
            let base = this.parent ? this.parent.get_world_transform() : qm.mat();
            qm.mat_mul(base, this.get_local_transform(), base);
            return base;
        }
    }
    qf.scene_component = scene_component;
    class rect_primitve extends scene_component {
        render_c2d_impl(ctx) {
            ctx.fillStyle = this.fill_color;
            ctx.fillRect(-this.bounds.x / 2, -this.bounds.y / 2, this.bounds.x, this.bounds.y);
        }
    }
    qf.rect_primitve = rect_primitve;
    class damage_event {
        constructor(damage, instigator, dir = qm.v()) {
            this.damage = damage;
            this.instigator = instigator;
            this.dir = dir;
        }
    }
    qf.damage_event = damage_event;
    class actor {
        constructor() {
            this.components = [];
            this.on_take_damage = new qf.multicast_delegate();
        }
        is_valid() {
            return !!this.world;
        }
        destroy() {
            this.world.destroy_actor(this);
        }
        get_pos() {
            return this.root.get_pos();
        }
        getcmp(clazz) {
            return this.components.filter(c => c instanceof clazz);
        }
        getcmp_byname(name) {
            return this.components.filter(c => c.name === name);
        }
    }
    qf.actor = actor;
    function attach_cmp(owner, cmp, name = '') {
        cmp.owner = owner;
        cmp.name = name;
        owner.components.push(cmp);
        if (owner.world.has_begun_play) {
            cmp.begin_play();
        }
        return cmp;
    }
    qf.attach_cmp = attach_cmp;
    function attach_prim(owner, prim, { x = 0, y = 0, width = 10, height = 10, root = false, coll_mask = 0 /* none */, name = '' }) {
        prim.pos.x = x;
        prim.pos.y = y;
        prim.bounds.x = width;
        prim.bounds.y = height;
        prim.collision_mask = coll_mask;
        if (root)
            owner.root = prim;
        return attach_cmp(owner, prim, name);
    }
    qf.attach_prim = attach_prim;
    class world {
        constructor() {
            this.actors = [];
            this.timer = new timer();
            this.has_begun_play = false;
        }
        tick(delta) {
            this.timer.tick(delta);
            for (let actor of this.actors) {
                for (let cmp of actor.components) {
                    if (qu.has_method(cmp, 'tick')) 
                    // if (!(cmp instanceof qf.prmitive_component || cmp instanceof qi.slime_ai))
                    {
                        cmp.tick(delta);
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
        destroy_actor(actor) {
            let i = this.actors.indexOf(actor);
            this.actors.splice(i, 1);
            actor.world = undefined;
        }
        sweep_aabb(start, end, size, channel = 4294967295 /* all */, ignore) {
            const area = qm.make_aabb(start, end);
            const half_size = qm.scale(size, 0.5);
            let hits = [];
            if (channel & 2 /* geom */) {
                let hit = this.geometry.sweep_aabb(start, end, size);
                if (hit) {
                    hits.push(new hit_result(hit[0], hit[1]));
                }
            }
            if (channel === 2 /* geom */) {
                return hits[0];
            }
            for (let actor of this.actors) {
                if (ignore && qu.contains(ignore, actor)) {
                    continue;
                }
                if (actor.root) {
                    let actor_aabb = actor.root.get_aabb(half_size);
                    if (qm.overlap_aabb(area, actor_aabb) && (actor.root.collision_mask & channel)) {
                        let hit = qm.line_trace_aabb(start, end, actor_aabb);
                        if (hit) {
                            hits.push(new hit_result(hit[0], hit[1], actor));
                        }
                        else {
                            hits.push(new hit_result(start, qm.up, actor));
                        }
                    }
                }
            }
            return qu.upper_bound(hits, (a, b) => {
                return qm.mag_sqr(qm.sub(a.pos, start)) > qm.mag_sqr(qm.sub(b.pos, start));
            });
        }
    }
    qf.world = world;
    class timer_event {
        constructor(owner, type, delay, fn, actor, ctx) {
            this.owner = owner;
            this.type = type;
            this.delay = delay;
            this.fn = fn;
            this.actor = actor;
            this.ctx = ctx;
            this.fire_in = 0;
            this.fire_in = delay;
        }
        execute() {
            if (this.is_valid()) {
                this.fn.call(this.ctx);
            }
        }
        is_valid() {
            return this.actor && this.actor.is_valid();
        }
        reset() {
            this.fire_in += this.delay;
        }
    }
    qf.timer_event = timer_event;
    class timer {
        constructor() {
            this.events = [];
        }
        tick(delta) {
            for (let i = this.events.length - 1; i >= 0; --i) {
                let e = this.events[i];
                if (!e.is_valid()) {
                    this.events.splice(i, 1);
                    continue;
                }
                e.fire_in -= delta;
                if (e.fire_in <= 0) {
                    e.execute();
                    if (e.type == 0 /* once */)
                        this.events.splice(i, 1);
                    if (e.type == 1 /* repeat */)
                        e.reset();
                }
            }
        }
        add_timer(time, type, fn, ctx) {
            let actor;
            if (ctx instanceof qf.actor) {
                actor = ctx;
            }
            else {
                actor = ctx.owner;
            }
            let e = new timer_event(this, type, time, fn, actor, ctx);
            this.events.push(e);
            return e;
        }
        delay(delay, fn, ctx) {
            return this.add_timer(delay, 0 /* once */, fn, ctx);
        }
        every(timespan, fn, ctx) {
            return this.add_timer(timespan, 1 /* repeat */, fn, ctx);
        }
        throttle(delay, fn, ctx) {
            let lock = false;
            return ((...args) => {
                if (!lock) {
                    lock = true;
                    this.delay(delay, _ => lock = false, ctx);
                    fn.apply(ctx, args);
                }
            });
        }
    }
    qf.timer = timer;
})(qf || (qf = {}));
// render
var qr;
(function (qr) {
    function create_canvas(width, height, cb) {
        let c = document.createElement('canvas');
        c.width = width;
        c.height = height;
        let ctx = c.getContext('2d');
        ctx.imageSmoothingEnabled = false;
        if (cb) {
            cb(ctx, c);
        }
        return c;
    }
    qr.create_canvas = create_canvas;
    function is_img_loaded(img) {
        if (img instanceof HTMLImageElement) {
            return img && img.complete && img.naturalHeight !== 0;
        }
        return true;
    }
    qr.is_img_loaded = is_img_loaded;
    class sprite_data {
        constructor(image, negative, position, size) {
            this.image = image;
            this.negative = negative;
            this.position = position;
            this.size = size;
        }
        is_valid() {
            return qr.is_img_loaded(this.image);
        }
    }
    qr.sprite_data = sprite_data;
    class spritesheet {
        constructor(image, negative_image, cell_size, grid_size) {
            this.image = image;
            this.negative_image = negative_image;
            this.cell_size = cell_size;
            this.grid_size = grid_size;
        }
        get_sprite(id) {
            let x = id % this.grid_size.x, y = Math.floor(id / this.grid_size.x);
            return new sprite_data(this.image, this.negative_image, qm.mul(qm.v(x, y), this.cell_size), this.cell_size);
        }
    }
    qr.spritesheet = spritesheet;
    class sprite_sequence {
        constructor(spritesheet, frames, durations) {
            this.spritesheet = spritesheet;
            this.frames = frames;
            this.durations = durations;
            this.loop = false;
            this.elapsed = 0;
            this.total_time = 0;
            if (this.durations) {
                qu.assert(this.frames.length == this.durations.length);
                this.total_time = this.durations.reduce((p, c) => p + c);
            }
            else {
                this.set_duration(1);
            }
        }
        set_duration(time) {
            qu.assert(time > 0);
            this.durations = this.frames.map(_ => time / this.frames.length);
            this.total_time = this.durations.reduce((p, c) => p + c);
        }
        tick(delta) {
            this.elapsed += delta;
            if (this.loop) {
                if (this.elapsed > this.total_time) {
                    this.elapsed -= this.total_time;
                }
            }
        }
        get_current_frame() {
            for (let i = 0, time = 0; i < this.durations.length; ++i) {
                time += this.durations[i];
                if (this.elapsed < time) {
                    return this.spritesheet.get_sprite(this.frames[i]);
                }
            }
            return this.spritesheet.get_sprite(this.frames[this.frames.length - 1]);
        }
    }
    qr.sprite_sequence = sprite_sequence;
    function render_w(ctx, world) {
        render_a(ctx, world.actors);
    }
    qr.render_w = render_w;
    function render_a(ctx, actors) {
        let primitives = [];
        for (let actor of actors) {
            for (let cmp of actor.components) {
                if (cmp instanceof qf.scene_component)
                    primitives.push(cmp);
            }
        }
        render_p(ctx, primitives);
    }
    qr.render_a = render_a;
    function render_p(ctx, prmitives) {
        for (let prim of prmitives) {
            if (prim.visible) {
                prim.render_c2d(ctx);
            }
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
            function get_state(key) {
                let s = state[key];
                if (!s)
                    state[key] = s = new key_state();
                return s;
            }
            keyboard.get_state = get_state;
            function just_pressed(key, dt_ms = 20) {
                let ks = state[key];
                if (ks && ks.is_down) {
                    return Date.now() - ks.timestamp < dt_ms;
                }
                return false;
            }
            keyboard.just_pressed = just_pressed;
            function on_keydown(e) {
                let s = get_state(e.key);
                if (s.is_down)
                    return;
                s.is_down = true;
                s.timestamp = Date.now();
            }
            keyboard.on_keydown = on_keydown;
            function on_keyup(e) {
                get_state(e.key).is_down = false;
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
    const c_jump_calc_offsets = [
        qm.top_left, qm.up, qm.top_right,
        qm.left, qm.right
    ];
    // order of these offsets matter for algorithm execution
    const c_breadth_search_offsets = [
        qm.left, qm.right, qm.up,
        qm.top_left, qm.top_right, qm.bottom_left,
        qm.bottom_right, qm.down
    ];
    class tile_geometry {
        constructor(width, height, tile_size = 10) {
            this.width = width;
            this.height = height;
            this.tile_size = tile_size;
            //public blocking_tiles: boolean[][] = [];
            this.blocking_dist = [];
            this.floor_dist = [];
            this.jump_dist = [];
            // debug
            this.d_considered = [];
            this.d_hits = [];
            qu.assert(width > 0 && height > 0);
            for (let i = 0; i < height; ++i) {
                // this.blocking_tiles[i] = new Array(width).fill(false);
                this.blocking_dist[i] = new Array(width).fill(99);
                this.floor_dist[i] = new Array(width).fill(99);
                this.jump_dist[i] = new Array(width).fill(99);
            }
        }
        set_blocking(x, y, blocking) {
            // currently only support add blocking;
            qu.assert(blocking === true);
            let node = qm.v(x, y);
            if (this.is_valid(node)) {
                // this.blocking_tiles[y][x] = blocking;
                this.update_blocking_dist(node, 0);
                this.update_floor_dist(node);
                this.update_jump_dist(node, 5);
            }
            else {
                qu.assert(false, `${x} != [0, ${this.width}) || ${y} != [0, ${this.height})`);
            }
        }
        get_blocking_dist(n) {
            return this.is_valid(n) ? this.blocking_dist[n.y][n.x] : Number.MAX_VALUE;
        }
        get_floor_dist(n) {
            return this.is_valid(n) ? this.floor_dist[n.y][n.x] : Number.MAX_VALUE;
        }
        get_jump_dist(n) {
            return this.is_valid(n) ? this.jump_dist[n.y][n.x] : Number.MAX_VALUE;
        }
        is_valid(loc) {
            return loc.x >= 0 && loc.x < this.width && loc.y >= 0 && loc.y < this.height;
        }
        update_blocking_dist(node, dist) {
            let open = [[node, dist]];
            while (open.length) {
                let [node, dist] = open.shift();
                this.blocking_dist[node.y][node.x] = dist;
                for (let offset of c_jump_calc_offsets) {
                    let n = qm.add(node, offset);
                    if (this.is_valid(n)) {
                        if (this.blocking_dist[n.y][n.x] > dist + 1) {
                            this.blocking_dist[n.y][n.x] = dist + 1;
                            open.push([n, dist + 1]);
                        }
                    }
                }
            }
        }
        update_floor_dist(loc) {
            for (let pos = qm.vc(loc), dist = 0; pos.y >= 0; --pos.y, ++dist) {
                if (this.floor_dist[pos.y][pos.x] < dist) {
                    break;
                }
                this.floor_dist[pos.y][pos.x] = dist;
            }
        }
        update_jump_dist(root, range) {
            this.jump_dist[root.y][root.x] = 0;
            for (let y = root.y - 1; y >= root.y - range; --y) {
                for (let x = root.x - range; x < root.x + range; ++x) {
                    let n = qm.v(x, y);
                    if (this.is_valid(n)) {
                        let dist = qm.manhattan_dist(qm.sub(n, root));
                        if (this.jump_dist[n.y][n.x] > dist) {
                            this.jump_dist[n.y][n.x] = dist;
                        }
                    }
                }
            }
        }
        is_blocking(x, y) {
            if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
                return this.blocking_dist[y][x] === 0;
            }
            return false;
        }
        foreach_tile_along_path(start_loc, end_loc, fn) {
            let s = this.project(start_loc);
            let e = this.project(end_loc);
            let d = qm.sub(e, s);
            const dir = qm.sub(end_loc, start_loc);
            // if trace is straight horizonal or vertial line
            if (d.x == 0 || d.y == 0) {
                for (let y = s.y, x = s.x;;) {
                    if (fn(x, y))
                        return;
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
                    if (fn(x, y))
                        return;
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
        line_trace2(start_loc, end_loc) {
            let hit_result;
            this.foreach_tile_along_path(start_loc, end_loc, (x, y) => {
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
        line_trace_tile(start, end, x, y) {
            const ts = this.tile_size;
            return qm.line_trace_aabb(start, end, [qm.scale(qm.v(x + 0.5, y + 0.5), ts), qm.v(ts / 2, ts / 2)]);
        }
        sweep_aabb(start, end, size) {
            let size_on_grid = qm.v(Math.max(0, Math.ceil(size.x / this.tile_size) + 1), Math.max(0, Math.ceil(size.y / this.tile_size) + 1));
            let ext = qm.v(Math.floor(size_on_grid.x / 2), Math.floor(size_on_grid.y / 2));
            const ts = this.tile_size;
            const half_ts = qm.scale(qm.v(ts, ts), 0.5);
            const half_size = qm.scale(size, 0.5);
            const dir = qm.unit(qm.sub(end, start));
            let hit_result;
            this.foreach_tile_along_path(start, end, (x, y) => {
                let hits = [];
                for (let iy = y - ext.y; iy <= y + ext.y; ++iy) {
                    for (let ix = x - ext.x; ix <= x + ext.x; ++ix) {
                        let tile_center = qm.scale(qm.v(ix + 0.5, iy + 0.5), ts);
                        let tile_dir = qm.unit(qm.sub(tile_center, start));
                        if (qm.dot(dir, tile_dir) < -0.5) {
                            continue;
                        }
                        if (this.d_considered)
                            this.d_considered.push(qm.scale(qm.v(ix, iy), ts));
                        if (this.is_blocking(ix, iy)) {
                            let tile_aabb = [tile_center, qm.add(half_size, half_ts)];
                            if (qm.overlap_point(start, tile_aabb)) {
                                let d = qm.sub(start, end);
                                let hit = qm.line_trace_aabb(qm.add(start, qm.scale(d, 999)), start, tile_aabb);
                                qu.assert(!!hit);
                                hits.push(hit);
                                continue;
                            }
                            let hit = qm.line_trace_aabb(start, end, tile_aabb);
                            if (hit) {
                                this.d_hits.push(hit);
                                hits.push(hit);
                            }
                        }
                    }
                }
                let best_dist = Number.MAX_VALUE;
                for (let hit of hits) {
                    let [p, _] = hit;
                    let dist = qm.mag_sqr(qm.sub(p, start));
                    if (dist < best_dist) {
                        best_dist = dist;
                        hit_result = hit;
                    }
                }
                if (hit_result)
                    return true;
                else
                    return false;
            });
            return hit_result;
        }
        find_path(start, end, is_conn_allowed) {
            let s = this.project(start);
            let e = this.project(end);
            let id = (v) => v.y * this.width + v.x;
            let open = [s];
            let visited = [id(s)];
            let prev = [-1];
            while (open.length) {
                let start = open.shift();
                if (qm.eq(start, e))
                    break;
                for (let offset of c_breadth_search_offsets) {
                    let n_loc = qm.add(start, offset);
                    if (!this.is_valid(n_loc)) {
                        continue;
                    }
                    if (is_conn_allowed(this, start, n_loc)) {
                        let n_id = id(n_loc);
                        if (!qu.contains(visited, n_id)) {
                            open.push(n_loc);
                            visited.push(n_id);
                            prev.push(id(start));
                        }
                    }
                }
            }
            let id_to_loc = (id) => {
                return qm.scale(qm.v(id % this.width + 0.5, Math.floor(id / this.width) + 0.5), this.tile_size);
            };
            let path = [id_to_loc(id(e))];
            for (let idx = visited.indexOf(id(e)); idx != -1;) {
                let tile_id = prev[idx];
                if (tile_id == -1) {
                    break;
                }
                path.unshift(id_to_loc(tile_id));
                idx = visited.indexOf(tile_id);
            }
            // paths.push(path);
            return path.length > 1 ? path : undefined;
        }
        project(point) {
            return qm.v(Math.floor(point.x / this.tile_size), Math.floor(point.y / this.tile_size));
        }
    }
    ql.tile_geometry = tile_geometry;
})(ql || (ql = {}));
// components
var qc;
(function (qc) {
    class character_movement extends qf.component_base {
        constructor() {
            super(...arguments);
            // setup
            // any axis
            this.max_velocity = 300;
            // x axis
            this.max_velocity_on_ground = 100;
            this.gravity = 1000;
            this.bounce_off_wall = false;
            // runtime data
            this.vel = qm.v();
            this.acc = qm.v();
            this.on_ground = false;
            this.moving_left = false;
        }
        trace_wall(dist = 2) {
            let r = this.owner.root;
            let g = this.owner.world.geometry;
            let trace = g.sweep_aabb(r.pos, qm.add(r.pos, qm.scale(qm.left, dist)), r.bounds);
            return trace ? trace : g.sweep_aabb(r.pos, qm.add(r.pos, qm.scale(qm.right, dist)), r.bounds);
        }
        trace_ground() {
            let r = this.owner.root;
            let g = this.owner.world.geometry;
            return g.sweep_aabb(r.pos, qm.add(r.pos, qm.down), r.bounds);
        }
        tick(delta) {
            let r = this.owner.root;
            let g = this.owner.world.geometry;
            this.vel.y += this.gravity * delta;
            this.vel = qm.add(this.vel, qm.scale(this.acc, delta));
            this.vel = qm.clamp_mag(this.vel, 0, this.max_velocity);
            this.vel.x = qm.clamp(this.vel.x, -this.max_velocity_on_ground, this.max_velocity_on_ground);
            let end = qm.add(r.pos, qm.scale(this.vel, delta));
            let trace = g.sweep_aabb(r.pos, end, r.bounds);
            if (trace) {
                let [p, n] = trace;
                this.last_hit = trace;
                if (n.y != 0) {
                    end.y = p.y;
                    if (end.x != p.x) {
                        let x_trace = g.sweep_aabb(p, end, r.bounds);
                        if (x_trace) {
                            end.x = x_trace[0].x;
                        }
                    }
                    this.vel.y = 0;
                }
                if (n.x != 0) {
                    end.x = p.x;
                    if (end.y != p.y) {
                        let y_trace = g.sweep_aabb(p, end, r.bounds);
                        if (y_trace) {
                            end.y = y_trace[0].y;
                        }
                    }
                    this.vel.x *= this.bounce_off_wall ? -qm.rnd(0.9, 1) : 0;
                }
            }
            // else {
            // r.pos.x = Math.round(end.x);
            r.pos.x = end.x;
            r.pos.y = end.y;
            // r.pos = end;
            // }
            this.on_ground = !!this.trace_ground();
        }
    }
    qc.character_movement = character_movement;
    class sprite_component extends qf.scene_component {
        constructor() {
            super(...arguments);
            this.sprite = new qr.sprite_data(null, null, qm.zero, qm.v(10, 10));
            this.flip_x = false;
            this.negative = false;
        }
        render_c2d_impl(ctx) {
            const sp = this.sprite.position, ss = this.sprite.size;
            if (this.flip_x) {
                ctx.scale(-1, 1);
            }
            if (this.offset) {
                ctx.translate(this.offset.x, this.offset.y);
            }
            if (this.sprite.is_valid()) {
                ctx.drawImage(this.negative ? this.sprite.negative : this.sprite.image, sp.x, sp.y, ss.x, ss.y, -ss.x / 2, -ss.y / 2, ss.x, ss.y);
            }
            else {
                ctx.fillStyle = 'red';
                ctx.fillRect(-ss.x / 2, -ss.y / 2, ss.x, ss.y);
            }
        }
    }
    qc.sprite_component = sprite_component;
    class anim_sprite_component extends qc.sprite_component {
        constructor() {
            super(...arguments);
            this.sequences = {};
            this.current_sequence = '';
        }
        play(name) {
            if (this.current_sequence != name) {
                qu.assert(!!this.sequences[name]);
                this.current_sequence = name;
                this.sprite = this.sequences[name].get_current_frame();
            }
        }
        tick(delta) {
            if (this.current_sequence) {
                let seq = this.sequences[this.current_sequence];
                seq.tick(delta);
                this.sprite = seq.get_current_frame();
            }
        }
    }
    qc.anim_sprite_component = anim_sprite_component;
})(qc || (qc = {}));
// ai
var qi;
(function (qi) {
    qi.g_paths = [];
    function default_walk_filter(geom, s, e) {
        if (geom.is_blocking(s.x, s.y))
            return false;
        let e_dist = geom.get_blocking_dist(e);
        // end is blocking tile
        if (e_dist == 0)
            return false;
        let dir = qm.sub(e, s);
        let e_floor = geom.get_floor_dist(e);
        // we can move horizontaly only on tiles that are floors
        if (dir.y == 0 && e_floor > 1)
            return false;
        let s_jump = geom.get_jump_dist(s);
        // if we want to go up
        if (dir.y < 0) {
            // we can go straight up only near walls
            if (dir.x == 0 && e_dist > 1)
                return false;
            // we can keep goin up while we are near floor or walls
            if (dir.x != 0 && s_jump > 3)
                return false;
        }
        return true;
    }
    qi.default_walk_filter = default_walk_filter;
    class ai_movement extends qc.character_movement {
        constructor() {
            super(...arguments);
            // setup
            this.on_ground_acc = 500;
            this.jump_vel = 300;
            // runtime
            this.input = qm.v();
        }
        tick(delta) {
            if (this.input.x != 0) {
                this.acc.x = this.input.x * this.on_ground_acc;
            }
            else if (this.on_ground) {
                this.vel.x *= 0.8;
            }
            if (this.input.y < 0) {
                let wall_hit = this.trace_wall(3);
                if (this.on_ground && !wall_hit) {
                    this.vel.y = -this.jump_vel;
                }
                else if (wall_hit) {
                    let [p, n] = wall_hit;
                    if (n.x > 0) {
                        this.vel.y = -this.jump_vel;
                        this.vel.x = 80;
                    }
                    else if (n.x < 0) {
                        this.vel.y = -this.jump_vel;
                        this.vel.x = -80;
                    }
                }
            }
            super.tick(delta);
        }
    }
    qi.ai_movement = ai_movement;
    class enemy_controller extends qf.component_base {
        constructor() {
            super(...arguments);
            this.hitpoints = 1;
            this.max_hitpoints = 1;
            this.base_damage = 1;
            this.atk_speed = 0.5;
            this.atk_lock = false;
        }
        begin_play() {
            super.begin_play();
            this.target = this.owner.world.player;
            this.root = this.owner.root;
            this.owner.on_take_damage.bind(this.take_damage, this);
            this.get_timer().every(0.1, this.update, this);
        }
        update() {
            if (this.overlaps_target() && !this.atk_lock) {
                let dir = qm.sub(this.target.get_pos(), this.owner.get_pos());
                this.target.on_take_damage.broadcast(new qf.damage_event(this.base_damage, this.owner, dir));
                this.get_timer().delay(this.atk_speed, _ => this.atk_lock = false, this);
                this.atk_lock = true;
            }
        }
        take_damage(e) {
            this.hitpoints -= e.damage;
            if (this.root instanceof qc.sprite_component) {
                let r = this.root;
                r.negative = true;
                // r.visible = false;
                this.get_timer().delay(0.05, _ => { r.negative = false; r.visible = false; }, this);
                this.get_timer().delay(0.10, _ => { r.negative = false; r.visible = true; }, this);
            }
            if (this.hitpoints <= 0) {
                this.handle_death();
            }
        }
        handle_death() {
            let c = this.get_world().actors.filter(a => a.getcmp(qi.humanoid_ai)[0]);
            if (c.length === 1) {
                qg.g_stage += 1;
                for (let i = 0; i < qg.g_stage; ++i) {
                    qi.spawn_humanoid(this.get_world(), { x: qm.rnd(30, 340), y: 30 });
                }
            }
            qg.g_time_dilation = 0.5;
            setTimeout(_ => qg.g_time_dilation = 1, 1000);
            this.owner.destroy();
        }
        overlaps_target() {
            return qm.overlap_aabb(this.root.get_aabb(), this.target.root.get_aabb());
        }
    }
    qi.enemy_controller = enemy_controller;
    class slime_ai extends enemy_controller {
        begin_play() {
            super.begin_play();
            [this.mov] = this.owner.getcmp(qc.character_movement);
            this.mov.bounce_off_wall = true;
            this.get_world().timer.delay(qm.rnd(), _ => {
                this.get_world().timer.every(qm.rnd(2.6, 3), this.do_jump.bind(this), this.owner);
            }, this.owner);
        }
        do_jump() {
            let dir = qm.sign(qm.sub(this.target.get_pos(), this.root.pos));
            let w = this.get_world();
            let start = this.root.pos;
            // debug
            qi.g_paths = [];
            let g = this.owner.world.geometry;
            let path = g.find_path(this.root.pos, this.target.root.pos, default_walk_filter);
            if (path) {
                for (let i = 1; i < path.length; ++i) {
                    dir = qm.sign(qm.sub(path[i], path[i - 1]));
                    if (dir.x != 0)
                        break;
                }
            }
            let jump = qm.scale(qm.v(300 * dir.x, -300), qm.rnd(0.5, 1));
            let hit = w.sweep_aabb(start, qm.add(start, qm.v(20 * dir.x, -20)), this.root.bounds, 2 /* geom */);
            this.mov.vel = hit ? qm.mul(jump, qm.v(0.2, 1.5)) : jump;
        }
    }
    qi.slime_ai = slime_ai;
    class humanoid_ai extends enemy_controller {
        constructor() {
            super(...arguments);
            this.dimishing_return = 1;
        }
        begin_play() {
            super.begin_play();
            this.mov = this.owner.getcmp(qi.ai_movement)[0];
            // this.mov.max_velocity_on_ground = 100    ;
            this.think_event = this.get_timer().every(0.033, this.think, this);
            // this.getworld().timer.delay(qm.rnd(0, 1), _ => {
            //     this.getworld().timer.every(0.1, this.btick.bind(this))
            // });
        }
        think(delta) {
            this.dimishing_return = qm.clamp(this.dimishing_return + 0.01, 0, 1);
            let g = this.owner.world.geometry;
            let path = g.find_path(this.root.pos, this.target.root.pos, default_walk_filter);
            if (path) {
                this.mov.input = qm.scale(qm.sign(qm.sub(path[1], path[0])), qm.rnd(0.5, 1));
            }
            else {
                this.mov.input.x = 0;
                this.mov.input.y = 0;
            }
        }
        take_damage(e) {
            this.think_event.fire_in = 0.5 * this.dimishing_return;
            this.mov.input = qm.v();
            this.mov.vel.x -= 1000 * e.dir.x * this.dimishing_return;
            this.mov.vel.y -= 300 * this.dimishing_return;
            this.dimishing_return = qm.clamp(this.dimishing_return - 0.1, 0, 1);
            super.take_damage(e);
        }
    }
    qi.humanoid_ai = humanoid_ai;
    function spawn_slime(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf.attach_prim(a, new qf.rect_primitve(), { x, y, coll_mask: 8 /* pawn */, root: true });
        r.fill_color = 'blue';
        qf.attach_cmp(a, new ai_movement());
        qf.attach_cmp(a, new slime_ai());
        return a;
    }
    qi.spawn_slime = spawn_slime;
    function spawn_humanoid(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf.attach_prim(a, new qc.anim_sprite_component(), { x, y, coll_mask: 8 /* pawn */, root: true });
        r.sprite = qg.g_character_spritesheet.get_sprite(19);
        let mov = qf.attach_cmp(a, new ai_movement());
        let h = qf.attach_cmp(a, new humanoid_ai());
        h.hitpoints = 8;
        h.max_hitpoints = 3;
        return a;
    }
    qi.spawn_humanoid = spawn_humanoid;
})(qi || (qi = {}));
var qg;
(function (qg) {
    qg.g_spritesheet_image = (_ => { let i = new Image(); i.src = 's.png'; return i; })();
    qg.g_negative_spritesheet_image = qr.create_canvas(128, 128);
    qg.g_character_spritesheet = new qr.spritesheet(qg.g_spritesheet_image, qg.g_negative_spritesheet_image, qm.v(14, 18), qm.v(9, 9));
    qg.g_tile_spritesheet = new qr.spritesheet(qg.g_spritesheet_image, qg.g_negative_spritesheet_image, qm.v(10, 10), qm.v(10, 10));
    qg.g_stage = 1;
    class player_movement extends qc.character_movement {
        process_input() {
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
            else if (this.on_ground) {
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
                        let r = this.owner.root;
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
        tick(delta) {
            this.process_input();
            super.tick(delta);
        }
    }
    class projectile_movement extends qf.component_base {
        constructor() {
            super(...arguments);
            this.acc = qm.v(0, 1000);
            this.vel = qm.v();
            this.lifespan = 0;
            this.on_hit = new qf.multicast_delegate();
        }
        begin_play() {
            if (this.lifespan > 0) {
                this.get_timer().delay(this.lifespan, _ => this.owner.destroy(), this.owner);
            }
        }
        tick(delta) {
            let r = this.owner.root;
            let w = this.owner.world;
            this.vel = qm.add(this.vel, qm.scale(this.acc, delta));
            let ds = qm.scale(this.vel, delta);
            let end = qm.add(r.pos, ds);
            let hit_result = w.sweep_aabb(r.pos, end, r.bounds, 4294967295 /* all */, [w.player]);
            if (hit_result) {
                this.vel = qm.v();
                this.acc = qm.v();
                r.pos = qm.vc(hit_result.pos);
                this.tick = undefined;
                this.on_hit.broadcast(hit_result, this);
            }
            else {
                r.pos = end;
                r.rot = Math.atan2(this.vel.y, this.vel.x);
            }
        }
    }
    qg.g_draw_considered = false;
    qg.g_draw_blocking_dist = false;
    qg.g_draw_floor_dist = false;
    qg.g_draw_jump_dist = false;
    qg.g_draw_id = false;
    qg.g_draw_bounds = false;
    class debug_draw_collisions extends qf.scene_component {
        begin_play() {
            this.tiles = this.owner.world.geometry;
        }
        render_c2d_impl(ctx) {
            const tl = this.tiles.tile_size;
            ctx.strokeStyle = 'white';
            ctx.fillStyle = 'red';
            // let to_local = this.get_world_transform();
            // qm.mat_invert(to_local);
            // const pos = qm.transform(this.query_start.root.pos, to_local)
            // const end = qm.transform(qs.input.mouse.pos, to_local);
            for (let x = 0; x < this.tiles.width; ++x) {
                for (let y = 0; y < this.tiles.height; ++y) {
                    if (qg.g_draw_bounds) {
                        if (this.tiles.is_blocking(x, y)) {
                            ctx.strokeRect(x * tl, y * tl, tl, tl);
                            // this.tiles.line_trace_tile(pos, end, x, y);
                        }
                    }
                    if (qg.g_draw_blocking_dist) {
                        ctx.fillStyle = 'red';
                        ctx.fillText(this.tiles.blocking_dist[y][x].toFixed(0), x * tl, (y + 1) * tl);
                    }
                    if (qg.g_draw_floor_dist) {
                        ctx.fillStyle = 'green';
                        ctx.fillText(this.tiles.floor_dist[y][x].toFixed(0), (x + 0.5) * tl, (y + 1) * tl);
                    }
                    if (qg.g_draw_jump_dist) {
                        ctx.fillStyle = 'pink';
                        ctx.fillText(this.tiles.jump_dist[y][x].toFixed(0), (x) * tl, (y + 0.5) * tl);
                    }
                    if (qg.g_draw_id) {
                        ctx.fillStyle = 'red';
                        ctx.fillText(x.toFixed(0), (x) * tl, (y + 0.5) * tl);
                        ctx.fillText(y.toFixed(0), (x + 0.5) * tl, (y + 1) * tl);
                    }
                }
            }
            if (qg.g_draw_bounds) {
                ctx.strokeStyle = 'pink';
                for (let act of this.owner.world.actors) {
                    for (let p of act.getcmp(qf.scene_component)) {
                        let h = qm.scale(p.bounds, 0.5);
                        ctx.strokeRect(p.pos.x - h.x, p.pos.y - h.y, h.x * 2, h.y * 2);
                    }
                }
            }
            ctx.strokeStyle = 'green';
            for (let path of qi.g_paths) {
                if (path.length == 0)
                    continue;
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
            if (qg.g_draw_considered)
                for (let r of considered)
                    ctx.strokeRect(r.x, r.y, tl, tl);
            // let hits = this.tiles.d_hits;
            // for (let [p, n] of hits) ctx.fillRect(p.x - 2, p.y - 2, 4, 4);
            this.tiles.d_considered = [];
            this.tiles.d_hits = [];
        }
    }
    function spawn_projectile(world, loc, dir, { lifespan = 0, gravity = 1000 }) {
        let a = world.spawn_actor();
        let r = qf.attach_prim(a, new qc.anim_sprite_component(), { root: true });
        r.pos = loc;
        r.bounds = qm.v(8, 6);
        r.sequences['fire'] = new qr.sprite_sequence(qg.g_character_spritesheet, [27, 28], [0.02, 1]);
        r.sequences['explode'] = new qr.sprite_sequence(qg.g_character_spritesheet, [29, 30, qm.rnd_select(31, 32), 8], [0.03, 0.03, 0.05, 1]);
        r.play('fire');
        let p = new projectile_movement();
        p.vel = dir;
        p.acc.y = gravity;
        p.lifespan = lifespan;
        p.on_hit.bind(_ => r.play('explode'), r);
        qf.attach_cmp(a, p);
        return a;
    }
    class player_controller extends qf.component_base {
        constructor() {
            super(...arguments);
            this.z_press_start = -1;
            this.last_fire_time = 0;
        }
        begin_play() {
            [this.movement] = this.owner.getcmp(player_movement);
            [this.sprite] = this.owner.getcmp(qc.anim_sprite_component);
            [this.weapon_sprite] = this.owner.getcmp_byname('weapon_sprite');
            this.fire_delegate = this.get_world().timer.throttle(0.15, this.fire, this);
            this.owner.on_take_damage.bind(this.take_damage, this);
            // this.get_timer().every(4, _ => {
            //     if (qm.rnd() > 0.2) {
            //         qi.spawn_humanoid(this.get_world(), {x: 30, y: 40});
            //     } else {
            //         qi.spawn_slime(this.get_world(), {x:100, y:40});
            //     }
            // }, this);
        }
        tick(delta) {
            let a = this.owner;
            this.sprite.flip_x = !this.movement.moving_left;
            this.weapon_sprite.pos.x = this.movement.moving_left ? -10 : 10;
            this.weapon_sprite.flip_x = !this.movement.moving_left;
            this.weapon_sprite.rot *= 0.9;
            let vel = this.movement.vel;
            if (this.movement.on_ground) {
                if (qm.eq_eps(vel.x, 0, 3)) {
                    this.sprite.play('idle');
                }
                else {
                    this.sprite.play('walk');
                }
                // g_time_dilation = 1;
            }
            else {
                this.sprite.play('jump');
                // g_time_dilation = 0.5;
            }
            if (qs.input.keyboard.is_down('z')) {
                this.fire_delegate();
            }
            // this.getworld().timer.throttle(0.1, (i: number) => i, this);
            // let z_state = qs.input.keyboard.get_state('z');
            // if (z_state.is_down)
            // {
            //     this.z_press_start = qs.input.keyboard.get_state('z').timestamp;
            // }
            // if (this.z_press_start > 0)
            // {
            //     const press_time = Date.now() - this.z_press_start;
            //     const can_fire = this.last_fire_time - this.z_press_start != 0;
            //     if (can_fire && (!z_state.is_down|| press_time > 1000)) {
            //         this.last_fire_time = this.z_press_start;
            //         this.z_press_start = 0;
            //         let ampl = qm.clamp(press_time / 100, 1, 10);
            //         let dir = qm.clamp_mag(qm.add(
            //                 qm.scale(this.movement.moving_left ? qm.left : qm.right, 100 * ampl),
            //                 qm.v(0, 0)), 0, 800);
            //         spawn_projectile(a.world, a.root.pos, dir, {lifespan: 0.6, gravity: 0});
            //     }
            // }
        }
        take_damage(e) {
            reset();
        }
        fire() {
            let a = this.owner;
            let dir = this.movement.moving_left ? qm.left : qm.right;
            dir = qm.scale(dir, 400);
            let b = spawn_projectile(a.world, this.weapon_sprite.get_pos(), dir, { lifespan: 0.3, gravity: 0 });
            b.getcmp(projectile_movement)[0].on_hit.bind(this.on_bullet_hit, this);
            this.movement.vel.x += dir.x * -0.1;
            this.weapon_sprite.rot = this.movement.moving_left ? 3.14 / 2 : -3.14 / 2;
        }
        on_bullet_hit(hit, bullet) {
            if (hit.actor) {
                hit.actor.on_take_damage.broadcast(new qf.damage_event(1, this.owner, hit.normal));
                // let m = hit.actor.getcmp(qc.character_movement)[0];
                // m.vel.x -= hit.normal.x * 1000;
                // m.vel.y -= 500;
            }
        }
    }
    function spawn_player(world, { x, y }) {
        let a = world.spawn_actor();
        let s = new qc.anim_sprite_component();
        let idle = s.sequences['idle'] = new qr.sprite_sequence(qg.g_character_spritesheet, [0, 1]);
        idle.loop = true;
        s.sequences['jump'] = new qr.sprite_sequence(qg.g_character_spritesheet, [9]);
        s.sequences['falling'] = new qr.sprite_sequence(qg.g_character_spritesheet, [0]);
        let walk = s.sequences['walk'] = new qr.sprite_sequence(qg.g_character_spritesheet, [9, 10, 11, 10]);
        walk.loop = true;
        walk.set_duration(0.3);
        s.play('walk');
        s.offset = qm.v(0, -1);
        qf.attach_prim(a, s, { x, y, width: 10, height: 14, coll_mask: 8 /* pawn */, root: true });
        let pistol = qf.attach_prim(a, new qc.sprite_component(), { x: 10, y: 0, name: 'weapon_sprite' });
        pistol.parent = s;
        pistol.flip_x = true;
        pistol.sprite = qg.g_character_spritesheet.get_sprite(18);
        qf.attach_cmp(a, new player_movement());
        qf.attach_cmp(a, new player_controller());
        let tdebug = new debug_draw_collisions();
        qf.attach_prim(a, tdebug, {});
        world.player = a;
        return a;
    }
    function spawn_tile(actor, { x, y }, id) {
        let s = qf.attach_prim(actor, new qc.sprite_component(), { x, y, width: 10, height: 10 });
        s.sprite = qg.g_tile_spritesheet.get_sprite(90 + parseInt(id, 16));
    }
    function parse_level(data, world) {
        let geom = world.geometry;
        let x = 0, y = 0;
        let tile_map = world.spawn_actor();
        for (let line of data.split('\n')) {
            x = 0;
            for (let char of line) {
                let pos = qm.scale(qm.v(x + 0.5, y + 0.5), geom.tile_size);
                if (char === '0' || char === '1') {
                    geom.set_blocking(x, y, true);
                    spawn_tile(tile_map, pos, char);
                }
                if (char === '@')
                    spawn_player(world, pos);
                if (char === 's')
                    qi.spawn_slime(world, pos);
                if (char === 'h')
                    qi.spawn_humanoid(world, pos);
                x += 1;
            }
            y += 1;
        }
    }
    let world;
    let ctx;
    qg.g_time_dilation = 1;
    function tick() {
        world.tick(0.016 * qg.g_time_dilation);
        ctx.clearRect(0, 0, 410, 210);
        qr.render_w(ctx, world);
        window.requestAnimationFrame(tick);
    }
    function reset() {
        qg.g_stage = 1;
        if (world) {
            console.log('score: ', world.actors.filter(a => a.getcmp(qi.humanoid_ai)[0]).length);
        }
        let t = new ql.tile_geometry(40, 20, 10);
        world = new qf.world();
        world.geometry = t;
        parse_level(`0000000000000000000000000000000000000000
0                                      0
0                                      0
0                                      0
0                                      0
0                                      0
0                                      0
0        @                             0
0      000000                          0
0                     h                0
0                    0000      000     0
0                                      0
0                                      0
0                                      0
0                                      0
0                                      0
0             0000   00000    00000    0
0    000 000000                        0
0                                      0
1111111111111111111111111111111111111111`, world);
        world.has_begun_play = true;
        for (let a of world.actors)
            for (let c of a.components)
                c.begin_play();
    }
    function main() {
        let canvas = document.querySelector("#canvas");
        ctx = canvas.getContext("2d");
        ctx.imageSmoothingEnabled = false;
        ctx.translate(10.5, 10.5);
        ctx.scale(2, 2);
        {
            let c = qg.g_negative_spritesheet_image.getContext('2d');
            let { width, height } = qg.g_negative_spritesheet_image;
            c.drawImage(qg.g_spritesheet_image, 0, 0);
            c.fillStyle = 'white';
            c.globalCompositeOperation = 'source-in';
            c.fillRect(0, 0, width, height);
            // c.globalCompositeOperation = 'difference';
            // c.drawImage(g_spritesheet_image, 0, 0);
        }
        reset();
        qs.input.init(canvas);
        window.requestAnimationFrame(tick);
    }
    qg.main = main;
    function load() {
        if (qr.is_img_loaded(qg.g_spritesheet_image)) {
            qg.main();
        }
        else {
            setTimeout(load, 100);
        }
    }
    qg.load = load;
})(qg || (qg = {}));
qg.load();
