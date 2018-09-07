var __;
(function (__) {
    // math
    class qm_vec {
        constructor(x, y) {
            this.x = x;
            this.y = y;
        }
    }
    class qm_cvec {
        constructor(x, y) {
            this.x = x;
            this.y = y;
        }
    }
    const qm_one = new qm_cvec(1, 1);
    const qm_zero = new qm_cvec(0, 0);
    const qm_up = new qm_cvec(0, -1);
    const qm_down = new qm_cvec(0, 1);
    const qm_left = new qm_cvec(-1, 0);
    const qm_right = new qm_cvec(1, 0);
    const qm_top_left = new qm_cvec(-1, -1);
    const qm_top_right = new qm_cvec(1, -1);
    const qm_bottom_left = new qm_cvec(-1, 1);
    const qm_bottom_right = new qm_cvec(1, 1);
    function v2(x = 0, y = 0) { return new qm_vec(x, y); }
    function v2c(a) { return new qm_vec(a.x, a.y); }
    function qm_eq(a, b) { return a.x == b.x && a.y == b.y; }
    function qm_add(a, b) { return v2(a.x + b.x, a.y + b.y); }
    function qm_sub(a, b) { return v2(a.x - b.x, a.y - b.y); }
    function qm_mul(a, b) { return v2(a.x * b.x, a.y * b.y); }
    function qm_dot(a, b) { return a.x * b.x + a.y * b.y; }
    function qm_scale(a, s) { return v2(a.x * s, a.y * s); }
    function qm_cross(a, b) { return a.x * b.y - a.y * b.x; }
    function qm_sign(a) { return v2(Math.sign(a.x), Math.sign(a.y)); }
    function qm_mag_sqr(a) { return a.x * a.x + a.y * a.y; }
    function qm_mag(a) { return Math.sqrt(qm_mag_sqr(a)); }
    function qm_unit(a) { let m = qm_mag(a); return qm_scale(a, 1 / m); }
    function qm_clamp_mag(a, min, max) {
        let m = qm_mag(a);
        return m < min ? qm_scale(qm_unit(a), min) :
            m > max ? qm_scale(qm_unit(a), max) :
                a;
    }
    function qm_rotate(a, rad) {
        let c = Math.cos(rad);
        let s = Math.sin(rad);
        return v2(c * a.x - s * a.y, s * a.x + c * a.y);
    }
    function qm_manhattan_dist(a) { return Math.abs(a.x) + Math.abs(a.y); }
    // general math
    function qm_clamp(x, min, max) { return Math.max(Math.min(x, max), min); }
    function qm_eq_eps(a, b, eps = 0.01) { return Math.abs(a - b) < eps; }
    function qm_rnd(min = 0, max = 1) { return min + (max - min) * Math.random(); }
    function qm_rnd_select(...elements) { return elements[qm_rnd(0, elements.length) | 0]; }
    ;
    // The Box-Muller transform converts two independent uniform variates on (0, 1) into two standard Gaussian variates (mean 0, variance 1). 
    function qm_rnd_normal() {
        let u = 0, v = 0;
        while (u === 0)
            u = Math.random(); //Converting [0,1) to (0,1)
        while (v === 0)
            v = Math.random();
        let n = Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
        return qm_clamp(n / 10, -1, 1);
    }
    // a, b - any two points
    // return [center, half_size]
    function qm_make_aabb(a, b) {
        let tl = v2(Math.min(a.x, b.x), Math.min(a.y, b.y));
        let br = v2(Math.max(a.x, b.x), Math.max(a.y, b.y));
        return [qm_scale(qm_add(tl, br), 0.5), qm_scale(qm_sub(br, tl), 0.5)];
    }
    function qm_overlap_point(a, [c, ext]) {
        let d = qm_sub(a, c);
        return Math.abs(d.x) < ext.x && Math.abs(d.y) < ext.y;
    }
    function qm_overlap_aabb([ac, aext], [bc, bext]) {
        return qm_overlap_point(ac, [bc, qm_add(aext, bext)]);
    }
    function qm_line_trace_aabb(start, end, aabb) {
        let get_vertex = (i) => {
            let [center, ext] = aabb;
            const hx = [-ext.x, ext.x, ext.x, -ext.x];
            const hy = [-ext.y, -ext.y, ext.y, ext.y];
            return v2(center.x + hx[i % 4], center.y + hy[i % 4]);
        };
        const trace = qm_sub(end, start);
        let hits = new Array(4);
        for (let i = 0; i < 4; ++i) {
            let v1 = get_vertex(i), v2 = get_vertex(i + 1);
            let edge = qm_sub(v2, v1);
            // if trace start point is above edge
            if (qm_cross(edge, qm_sub(start, v1)) <= 0 &&
                // and end trace is below line
                qm_cross(edge, qm_sub(end, v1)) >= 0) {
                // trace is vertical line
                if (trace.x == 0) {
                    // we can only colide against horizontal line
                    if (i == 0 || i == 2) {
                        if (start.x > Math.min(v1.x, v2.x) && start.x < Math.max(v1.x, v2.x)) {
                            hits[i] = new qm_vec(start.x, v1.y);
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
                        hits[i] = new qm_vec(x, y);
                    }
                }
                // vertical line
                else {
                    let x = v1.x;
                    let y = a * x + b;
                    if (y > Math.min(v1.y, v2.y) && y < Math.max(v1.y, v2.y)) {
                        hits[i] = new qm_vec(x, y);
                    }
                }
            }
        }
        let best_dist = Number.MAX_VALUE;
        let best_index = -1;
        for (let i = 0; i < 4; ++i) {
            if (hits[i]) {
                let dist = qm_mag_sqr(qm_sub(hits[i], start));
                if (dist < best_dist) {
                    best_dist = dist;
                    best_index = i;
                }
            }
        }
        if (best_index != -1) {
            const normal = [qm_up, qm_right, qm_down, qm_left];
            return [hits[best_index], normal[best_index]];
        }
    }
    function qm_mat(m00 = 1, m10 = 0, m01 = 0, m11 = 1, tx = 0, ty = 0) {
        return new Float32Array([m00, m10, 0, m01, m11, 0, tx, ty, 1]);
    }
    function qm_transform(v, m) {
        return v2(v.x * m[0] + v.y * m[3] + m[6], v.x * m[1] + v.y * m[4] + m[7]);
    }
    function qm_mat_mul(a, b, out) {
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
    ;
    /**
     * @param out if doesn't provided it will override input matrix
     */
    function qm_mat_invert(a, out = a) {
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
    // utilites
    function qu_has_method(obj, name) {
        return typeof obj[name] == 'function';
    }
    function qu_assert(cond, msg = `Assert failed`) {
        // #DEBUG-BEGIN
        if (!cond)
            throw new Error(msg);
        // #DEBUG-END
    }
    function qu_contains(arr, elem) {
        return arr.indexOf(elem) >= 0;
    }
    function qu_greatest_element(arr, cmp) {
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
    class qf_multicast_delegate {
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
    class qf_hit_result {
        constructor(pos = qm_zero, normal = qm_up, actor = undefined) {
            this.pos = pos;
            this.normal = normal;
            this.actor = actor;
        }
    }
    function qf_has_tick_method(obj) {
        return obj.wants_tick;
    }
    class qf_component_base {
        constructor() {
            this.name = '';
            this.wants_tick = false;
        }
        begin_play() { }
        get_world() { return this.owner.world; }
        get_timer() { return this.owner.world.timer; }
        is_valid() { return this.owner && this.owner.is_valid(); }
    }
    class qf_scene_component extends qf_component_base {
        constructor() {
            super(...arguments);
            this.pos = v2(0, 0);
            this.scale = v2(1, 1);
            this.rot = 0;
            this.bounds = v2(0, 0);
            this.visible = true;
            this.collision_mask = 0 /* none */;
        }
        render_c2d(ctx) {
            let pos = this.pos;
            let scale = this.scale;
            if (this.parent) {
                let t = this.get_world_transform();
                pos = qm_transform(qm_zero, t);
                scale = qm_sub(qm_transform(qm_one, t), pos);
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
                return qm_transform(qm_zero, this.get_world_transform());
            }
            return v2c(this.pos);
        }
        get_aabb(ext = qm_zero) {
            return [v2c(this.pos), qm_add(qm_scale(qm_mul(this.bounds, this.scale), 0.5), ext)];
        }
        get_local_transform() {
            return qm_mat(this.scale.x, 0, 0, this.scale.y, this.pos.x, this.pos.y);
        }
        get_world_transform() {
            let base = this.parent ? this.parent.get_world_transform() : qm_mat();
            qm_mat_mul(base, this.get_local_transform(), base);
            return base;
        }
    }
    class qf_rect_primitve extends qf_scene_component {
        render_c2d_impl(ctx) {
            ctx.fillStyle = this.fill_color;
            ctx.fillRect(-this.bounds.x / 2, -this.bounds.y / 2, this.bounds.x, this.bounds.y);
        }
    }
    class qf_damage_event {
        constructor(damage, instigator, dir = v2()) {
            this.damage = damage;
            this.instigator = instigator;
            this.dir = dir;
        }
    }
    class qf_actor {
        constructor() {
            this.components = [];
            this.on_take_damage = new qf_multicast_delegate();
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
    function qf_attach_cmp(owner, cmp, name = '') {
        cmp.owner = owner;
        cmp.name = name;
        owner.components.push(cmp);
        if (owner.world.has_begun_play) {
            cmp.begin_play();
        }
        return cmp;
    }
    function qf_attach_prim(owner, prim, { x = 0, y = 0, width = 10, height = 10, root = false, coll_mask = 0 /* none */, name = '' }) {
        prim.pos.x = x;
        prim.pos.y = y;
        prim.bounds.x = width;
        prim.bounds.y = height;
        prim.collision_mask = coll_mask;
        if (root)
            owner.root = prim;
        return qf_attach_cmp(owner, prim, name);
    }
    class qf_world {
        constructor() {
            this.actors = [];
            this.timer = new qf_timer();
            this.has_begun_play = false;
        }
        tick(delta) {
            this.timer.tick(delta);
            for (let actor of this.actors) {
                for (let cmp of actor.components) {
                    if (qf_has_tick_method(cmp)) {
                        cmp.tick(delta);
                    }
                }
            }
        }
        spawn_actor() {
            let a = new qf_actor();
            a.world = this;
            this.actors.push(a);
            return a;
        }
        destroy_actor(actor) {
            let i = this.actors.indexOf(actor);
            this.actors.splice(i, 1);
            actor.world = undefined;
        }
        overlap(aabb, channel = 4294967295 /* all */) {
            return this.actors.filter(a => {
                return a.root &&
                    (a.root.collision_mask & channel) &&
                    qm_overlap_aabb(a.root.get_aabb(), aabb);
            });
        }
        sweep_aabb(start, end, size, channel = 4294967295 /* all */, ignore) {
            const area = qm_make_aabb(start, end);
            const half_size = qm_scale(size, 0.5);
            let hits = [];
            if (channel & 2 /* geom */) {
                let hit = this.geometry.sweep_aabb(start, end, size);
                if (hit) {
                    hits.push(new qf_hit_result(hit[0], hit[1]));
                }
            }
            if (channel === 2 /* geom */) {
                return hits[0];
            }
            for (let actor of this.actors) {
                if (ignore && qu_contains(ignore, actor)) {
                    continue;
                }
                if (actor.root) {
                    let actor_aabb = actor.root.get_aabb(half_size);
                    // if ((actor.root.collision_mask & channel) && qm_overlap_aabb(area, actor_aabb)) {
                    if (actor.root.collision_mask & channel) {
                        let hit = qm_line_trace_aabb(start, end, actor_aabb);
                        if (hit) {
                            hits.push(new qf_hit_result(hit[0], hit[1], actor));
                        }
                    }
                    // }
                }
            }
            return qu_greatest_element(hits, (a, b) => {
                return qm_mag_sqr(qm_sub(a.pos, start)) > qm_mag_sqr(qm_sub(b.pos, start));
            });
        }
    }
    class qf_timer_event {
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
    class qf_timer {
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
            if (ctx instanceof qf_actor) {
                actor = ctx;
            }
            else {
                actor = ctx.owner;
            }
            let e = new qf_timer_event(this, type, time, fn, actor, ctx);
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
    // render
    function qr_create_canvas(width, height, cb) {
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
    function qr_is_img_loaded(img) {
        if (img instanceof HTMLImageElement) {
            return img && img.complete && img.naturalHeight !== 0;
        }
        return true;
    }
    class qr_sprite_data {
        constructor(image, negative, position, size) {
            this.image = image;
            this.negative = negative;
            this.position = position;
            this.size = size;
        }
        is_valid() {
            return qr_is_img_loaded(this.image);
        }
    }
    class qr_spritesheet {
        constructor(image, negative_image, cell_size, grid_size) {
            this.image = image;
            this.negative_image = negative_image;
            this.cell_size = cell_size;
            this.grid_size = grid_size;
        }
        get_sprite(id) {
            let x = id % this.grid_size.x, y = Math.floor(id / this.grid_size.x);
            return new qr_sprite_data(this.image, this.negative_image, qm_mul(v2(x, y), this.cell_size), this.cell_size);
        }
    }
    class qr_sprite_sequence {
        constructor(spritesheet, frames, durations) {
            this.spritesheet = spritesheet;
            this.frames = frames;
            this.durations = durations;
            this.loop = false;
            this.elapsed = 0;
            this.total_time = 0;
            if (this.durations) {
                qu_assert(this.frames.length == this.durations.length);
                this.total_time = this.durations.reduce((p, c) => p + c);
            }
            else {
                this.set_duration(1);
            }
        }
        set_duration(time) {
            qu_assert(time > 0);
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
    function qr_render_w(ctx, world) {
        qr_render_a(ctx, world.actors);
    }
    function qr_render_a(ctx, actors) {
        let primitives = [];
        for (let actor of actors) {
            for (let cmp of actor.components) {
                if (cmp instanceof qf_scene_component)
                    primitives.push(cmp);
            }
        }
        qr_render_p(ctx, primitives);
    }
    function qr_render_p(ctx, prmitives) {
        for (let prim of prmitives) {
            if (prim.visible) {
                prim.render_c2d(ctx);
            }
        }
    }
    // system
    let qs_input;
    (function (qs_input) {
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
                e.preventDefault();
            }
            keyboard.on_keydown = on_keydown;
            function on_keyup(e) {
                get_state(e.key).is_down = false;
            }
            keyboard.on_keyup = on_keyup;
        })(keyboard = qs_input.keyboard || (qs_input.keyboard = {}));
        let mouse;
        (function (mouse) {
            mouse.pos = v2();
            function on_move(e) {
                mouse.pos.x = e.offsetX;
                mouse.pos.y = e.offsetY;
                e.stopPropagation();
            }
            mouse.on_move = on_move;
        })(mouse = qs_input.mouse || (qs_input.mouse = {}));
        function init(canvas) {
            window.addEventListener('keydown', keyboard.on_keydown);
            window.addEventListener('keyup', keyboard.on_keyup);
            canvas.addEventListener('mousemove', mouse.on_move);
        }
        qs_input.init = init;
    })(qs_input || (qs_input = {}));
    // geometry
    const qf_c_jump_calc_offsets = [
        qm_top_left, qm_up, qm_top_right,
        qm_left, qm_right
    ];
    // order of these offsets matter for algorithm execution
    const qf_c_breadth_search_offsets = [
        qm_left, qm_right, qm_up,
        qm_top_left, qm_top_right, qm_bottom_left,
        qm_bottom_right, qm_down
    ];
    class qf_tile_geometry {
        constructor(width, height, tile_size = 10) {
            this.width = width;
            this.height = height;
            this.tile_size = tile_size;
            //public blocking_tiles: boolean[][] = [];
            this.blocking_dist = [];
            this.floor_dist = [];
            this.jump_dist = [];
            this.d_considered = [];
            this.d_hits = [];
            qu_assert(width > 0 && height > 0);
            for (let i = 0; i < height; ++i) {
                // this.blocking_tiles[i] = new Array(width).fill(false);
                this.blocking_dist[i] = new Array(width).fill(99);
                this.floor_dist[i] = new Array(width).fill(99);
                this.jump_dist[i] = new Array(width).fill(99);
            }
        }
        set_blocking(x, y, blocking) {
            // currently only support add blocking;
            qu_assert(blocking === true);
            let node = v2(x, y);
            if (this.is_valid(node)) {
                // this.blocking_tiles[y][x] = blocking;
                this.update_blocking_dist(node, 0);
                this.update_floor_dist(node);
                this.update_jump_dist(node, 5);
            }
            else {
                qu_assert(false, `${x} != [0, ${this.width}) || ${y} != [0, ${this.height})`);
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
                for (let offset of qf_c_jump_calc_offsets) {
                    let n = qm_add(node, offset);
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
            for (let pos = v2c(loc), dist = 0; pos.y >= 0; --pos.y, ++dist) {
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
                    let n = v2(x, y);
                    if (this.is_valid(n)) {
                        let dist = qm_manhattan_dist(qm_sub(n, root));
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
            let d = qm_sub(e, s);
            const dir = qm_sub(end_loc, start_loc);
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
                // #DEBUG-BEGIN
                if (this.d_considered)
                    this.d_considered.push(qm_scale(v2(x, y), this.tile_size));
                // #DEBUG-END
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
            return qm_line_trace_aabb(start, end, [qm_scale(v2(x + 0.5, y + 0.5), ts), v2(ts / 2, ts / 2)]);
        }
        sweep_aabb(start, end, size) {
            let size_on_grid = v2(Math.max(0, Math.ceil(size.x / this.tile_size) + 1), Math.max(0, Math.ceil(size.y / this.tile_size) + 1));
            let ext = v2(Math.floor(size_on_grid.x / 2), Math.floor(size_on_grid.y / 2));
            const ts = this.tile_size;
            const half_ts = qm_scale(v2(ts, ts), 0.5);
            const half_size = qm_scale(size, 0.5);
            const dir = qm_unit(qm_sub(end, start));
            let hit_result;
            this.foreach_tile_along_path(start, end, (x, y) => {
                let hits = [];
                for (let iy = y - ext.y; iy <= y + ext.y; ++iy) {
                    for (let ix = x - ext.x; ix <= x + ext.x; ++ix) {
                        let tile_center = qm_scale(v2(ix + 0.5, iy + 0.5), ts);
                        let tile_dir = qm_unit(qm_sub(tile_center, start));
                        if (qm_dot(dir, tile_dir) < -0.5) {
                            continue;
                        }
                        // #DEBUG-BEGIN
                        if (this.d_considered)
                            this.d_considered.push(qm_scale(v2(ix, iy), ts));
                        // #DEBUG-END
                        if (this.is_blocking(ix, iy)) {
                            let tile_aabb = [tile_center, qm_add(half_size, half_ts)];
                            if (qm_overlap_point(start, tile_aabb)) {
                                let d = qm_sub(start, end);
                                let hit = qm_line_trace_aabb(qm_add(start, qm_scale(d, 999)), start, tile_aabb);
                                qu_assert(!!hit);
                                hits.push(hit);
                                continue;
                            }
                            let hit = qm_line_trace_aabb(start, end, tile_aabb);
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
                    let dist = qm_mag_sqr(qm_sub(p, start));
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
                if (qm_eq(start, e))
                    break;
                for (let offset of qf_c_breadth_search_offsets) {
                    let n_loc = qm_add(start, offset);
                    if (!this.is_valid(n_loc)) {
                        continue;
                    }
                    let n_id = id(n_loc);
                    if (!qu_contains(visited, n_id)) {
                        if (is_conn_allowed(this, start, n_loc)) {
                            open.push(n_loc);
                            visited.push(n_id);
                            prev.push(id(start));
                        }
                    }
                }
            }
            let id_to_loc = (id) => {
                return qm_scale(v2(id % this.width + 0.5, Math.floor(id / this.width) + 0.5), this.tile_size);
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
            // #DEBUG-BEGIN
            if (__.g_draw_paths) {
                qi_g_paths.push(path);
            }
            // #DEBUG-END
            return path.length > 1 ? path : undefined;
        }
        project(point) {
            return v2(Math.floor(point.x / this.tile_size), Math.floor(point.y / this.tile_size));
        }
    }
    // components
    class qc_character_movement extends qf_component_base {
        constructor() {
            super(...arguments);
            // setup
            // any axis
            this.max_velocity = 300;
            // x axis
            this.max_velocity_on_ground = 100;
            this.gravity = 1000;
            this.bounce_off_wall = false;
            this.do_proces_input = true;
            // runtime data
            this.vel = v2();
            this.acc = v2();
            this.on_ground = false;
            this.moving_left = false;
            this.wants_tick = true;
        }
        trace_wall(dist = 2) {
            let r = this.owner.root;
            let g = this.owner.world.geometry;
            let trace = g.sweep_aabb(r.pos, qm_add(r.pos, qm_scale(qm_left, dist)), r.bounds);
            return trace ? trace : g.sweep_aabb(r.pos, qm_add(r.pos, qm_scale(qm_right, dist)), r.bounds);
        }
        trace_ground() {
            let r = this.owner.root;
            let g = this.owner.world.geometry;
            return g.sweep_aabb(r.pos, qm_add(r.pos, qm_down), r.bounds);
        }
        tick(delta) {
            if (this.do_proces_input) {
                this.process_input();
            }
            let r = this.owner.root;
            let g = this.owner.world.geometry;
            this.vel.y += this.gravity * delta;
            this.vel = qm_add(this.vel, qm_scale(this.acc, delta));
            this.vel = qm_clamp_mag(this.vel, 0, this.max_velocity);
            // if (this.on_ground) {
            this.vel.x = qm_clamp(this.vel.x, -this.max_velocity_on_ground, this.max_velocity_on_ground);
            // }
            let end = qm_add(r.pos, qm_scale(this.vel, delta));
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
                    this.vel.y *= this.bounce_off_wall ? -qm_rnd(0.2, 0.5) : 0;
                }
                if (n.x != 0) {
                    end.x = p.x;
                    if (end.y != p.y) {
                        let y_trace = g.sweep_aabb(p, end, r.bounds);
                        if (y_trace) {
                            end.y = y_trace[0].y;
                        }
                    }
                    this.vel.x *= this.bounce_off_wall ? -qm_rnd(0.2, 0.5) : 0;
                    // this.vel = qm_scale(this.vel, this.bounce_off_wall ? - qm_rnd(0.5, 0.7) : 0);
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
    class qc_sprite_component extends qf_scene_component {
        constructor() {
            super(...arguments);
            this.sprite = new qr_sprite_data(null, null, qm_zero, v2(10, 10));
            this.flip_x = false;
            this.offset = v2();
            this.negative = false;
        }
        blink() {
            this.visible = false;
            this.negative = true;
            this.get_timer().delay(0.05, _ => this.visible = true, this);
            this.get_timer().delay(0.10, _ => this.negative = false, this);
        }
        render_c2d_impl(ctx) {
            const sp = this.sprite.position, ss = this.sprite.size;
            if (this.flip_x) {
                ctx.scale(-1, 1);
            }
            if (this.sprite.is_valid()) {
                ctx.drawImage(this.negative ? this.sprite.negative : this.sprite.image, sp.x, sp.y, ss.x, ss.y, -ss.x / 2 + this.offset.x, -ss.y / 2 + this.offset.y, ss.x, ss.y);
            }
            else {
                ctx.fillStyle = 'red';
                ctx.fillRect(-ss.x / 2, -ss.y / 2, ss.x, ss.y);
            }
        }
    }
    class qc_anim_sprite_component extends qc_sprite_component {
        constructor() {
            super(...arguments);
            this.sequences = {};
            this.current_sequence = '';
            this.wants_tick = true;
        }
        play(name) {
            if (this.current_sequence != name) {
                qu_assert(!!this.sequences[name]);
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
    class qc_explosion_component extends qf_scene_component {
        constructor() {
            super(...arguments);
            this.wants_tick = true;
            this.duration = 0.11;
            this.elpased = 0;
            this.radious = 10;
        }
        tick(delta) {
            if (this.elpased > this.duration) {
                this.owner.destroy();
                g_scene_offset = v2();
            }
            g_scene_offset.x = qm_rnd(-1, 1);
            g_scene_offset.y = qm_rnd(-1, 1);
            this.elpased += delta;
        }
        render_c2d_impl(ctx) {
            ctx.beginPath();
            ctx.arc(0, 0, this.radious, 0, 2 * Math.PI);
            ctx.fillStyle = this.elpased / this.duration < 0.5 ? '#000' : '#fff';
            ctx.fill();
        }
    }
    class qc_wave_component extends qf_scene_component {
        constructor() {
            super(...arguments);
            this.wants_tick = true;
            this.start_radious = 0;
            this.end_radious = 1000;
            this.expand_durtation = 0.5;
            this.slomo_duration = 0.1;
            this.elapsed = 0;
        }
        begin_play() {
            // g_time_dilation = 0.1;
            // this.get_timer().delay(this.slomo_duration, _ => g_time_dilation = 1, this);
            let w = this.get_world();
            let t = this.get_timer();
            for (let a of w.actors) {
                let m = a.getcmp(qi_ai_movement)[0];
                if (a.root && m) {
                    let dir = qm_sub(a.get_pos(), this.pos);
                    let mag = qm_mag(dir);
                    dir = qm_scale(dir, 1000 / mag);
                    m.vel = dir;
                    m.do_proces_input = false;
                    let b = m.bounce_off_wall;
                    let ms = m.max_velocity;
                    m.bounce_off_wall = true;
                    m.max_velocity = 9000;
                    t.delay(0.1, _ => {
                        m.do_proces_input = true;
                        m.bounce_off_wall = b;
                        m.max_velocity = ms;
                    }, a);
                }
            }
        }
        tick(delta) {
            const hs = this.slomo_duration / 1.1;
            __.g_time_dilation = qm_clamp(1 - this.elapsed / hs, 0.01, 1);
            this.elapsed += delta;
            if (this.elapsed > this.slomo_duration) {
                __.g_time_dilation = 1;
                this.owner.destroy();
            }
        }
        render_c2d_impl(ctx) {
            let a = qm_clamp(this.elapsed / this.expand_durtation, 0, 1);
            ctx.beginPath();
            ctx.arc(0, 0, this.start_radious + (this.end_radious - this.start_radious) * a, 0, 2 * Math.PI);
            let gco = ctx.globalCompositeOperation;
            ctx.globalCompositeOperation = 'difference';
            ctx.strokeStyle = '#fff';
            ctx.lineWidth = 3;
            ctx.stroke();
            ctx.globalCompositeOperation = gco;
        }
    }
    class qc_pickable_component extends qf_component_base {
        begin_play() {
            this.get_timer().every(0.1, this.update, this);
        }
        update() {
            let r = this.owner.root;
            let w = this.get_world();
            let o = w.overlap(r.get_aabb(), 16 /* player */);
            if (o[0] && this.activate_delegate) {
                this.activate_delegate();
            }
        }
    }
    // ai
    var qi_g_paths = [];
    const qi_g_on_enemy_killed = new qf_multicast_delegate();
    function qi_flying_path_filter(geom, s, e) {
        return !geom.is_blocking(e.x, e.y);
    }
    function qi_default_walk_filter(geom, s, e) {
        if (geom.is_blocking(s.x, s.y))
            return false;
        let e_dist = geom.get_blocking_dist(e);
        // end is blocking tile
        if (e_dist == 0)
            return false;
        let dir = qm_sub(e, s);
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
            if (dir.x != 0 && s_jump > 2)
                return false;
        }
        return true;
    }
    class qi_ai_movement extends qc_character_movement {
        constructor() {
            super(...arguments);
            // setup
            this.ground_acc = 500;
            this.air_acc = 500;
            this.jump_vel = 300;
            this.flying = false;
            // runtime
            this.input = v2();
        }
        process_input() {
            if (this.flying) {
                this.acc = qm_scale(this.input, this.air_acc);
                return;
            }
            if (this.input.x != 0) {
                if (this.on_ground) {
                    this.acc.x = this.input.x * this.ground_acc;
                }
                else {
                    this.acc.x = this.input.x * this.air_acc;
                }
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
            else {
                this.acc.y = 0;
            }
        }
    }
    class qi_enemy_controller extends qf_component_base {
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
            [this.mov] = this.owner.getcmp(qi_ai_movement);
            [this.spr] = this.owner.getcmp(qc_anim_sprite_component);
            this.target = this.owner.world.player;
            this.root = this.owner.root;
            this.owner.on_take_damage.bind(this.take_damage, this);
            this.get_timer().every(0.1, this.update, this);
            this.get_timer().delay(0, _ => this.hitpoints += g_stage, this);
        }
        update() {
            if (this.mov && this.spr) {
                this.spr.flip_x = this.mov.vel.x > 0;
            }
            if (this.overlaps_target() && !this.atk_lock) {
                let dir = qm_sub(this.target.get_pos(), this.owner.get_pos());
                this.target.on_take_damage.broadcast(new qf_damage_event(this.base_damage, this.owner, dir));
                this.get_timer().delay(this.atk_speed, _ => this.atk_lock = false, this);
                this.atk_lock = true;
            }
        }
        take_damage(e) {
            this.last_hit = e;
            this.hitpoints -= e.damage;
            if (this.root instanceof qc_sprite_component) {
                this.root.blink();
            }
            if (this.hitpoints <= 0) {
                this.handle_death();
            }
        }
        handle_death() {
            let w = this.get_world();
            let c = w.actors.filter(a => a.getcmp(qi_enemy_controller)[0]);
            for (let i = qm_rnd(1, 3); i >= 0; --i) {
                // let coin = spawn_coin(w, qm_add(this.owner.get_pos(), v2(0, -5))).getcmp(qc_projectile_movement)[0];
                let coin = spawn_coin(w, this.owner.get_pos()).getcmp(qc_projectile_movement)[0];
                coin.vel.x = this.last_hit.dir.x * -qm_rnd(100, 200);
                coin.vel.y = qm_rnd(200, 400);
            }
            if (c.length === 1) {
                g_stage += 1;
                for (let i = 0; i < g_stage; ++i) {
                    this.get_timer().delay(qm_rnd(), _ => {
                        qm_rnd_select(qi_spawn_humanoid, qi_spawn_slime, spawn_bat)(w, { x: qm_rnd(30, 340), y: 30 });
                    }, this.target);
                }
            }
            qi_g_on_enemy_killed.broadcast(this.owner);
            this.owner.destroy();
        }
        overlaps_target() {
            return qm_overlap_aabb(this.root.get_aabb(), this.target.root.get_aabb());
        }
    }
    class qi_slime_controller extends qi_enemy_controller {
        begin_play() {
            super.begin_play();
            this.mov.bounce_off_wall = true;
            this.get_world().timer.delay(qm_rnd(), _ => {
                this.get_world().timer.every(qm_rnd(2.6, 3), this.do_jump.bind(this), this.owner);
            }, this.owner);
        }
        do_jump() {
            let dir = qm_sign(qm_sub(this.target.get_pos(), this.root.pos));
            let w = this.get_world();
            let start = this.root.pos;
            let g = this.owner.world.geometry;
            let path = g.find_path(this.root.pos, this.target.root.pos, qi_default_walk_filter);
            if (path) {
                for (let i = 1; i < path.length; ++i) {
                    dir = qm_sign(qm_sub(path[i], path[i - 1]));
                    if (dir.x != 0)
                        break;
                }
            }
            let jump = qm_scale(v2(300 * dir.x, -300), qm_rnd(0.5, 1));
            let hit = w.sweep_aabb(start, qm_add(start, v2(20 * dir.x, -20)), this.root.bounds, 2 /* geom */);
            this.mov.vel = hit ? qm_mul(jump, v2(0.2, 1.5)) : jump;
        }
    }
    class qi_humanoid_controller extends qi_enemy_controller {
        constructor() {
            super(...arguments);
            this.path_filter = qi_default_walk_filter;
            this.dimishing_return = 1;
        }
        begin_play() {
            super.begin_play();
            this.think_event = this.get_timer().every(0.033, this.think, this);
            // this.get_timer().every(1, this.try_fire, this);
        }
        think(delta) {
            this.dimishing_return = qm_clamp(this.dimishing_return + 0.01, 0, 1);
            let g = this.owner.world.geometry;
            let path = g.find_path(this.root.pos, this.target.root.pos, this.path_filter);
            if (path) {
                this.mov.input = qm_scale(qm_sign(qm_sub(path[1], path[0])), qm_rnd(0.5, 1));
            }
            else {
                this.mov.input.x = 0;
                this.mov.input.y = 0;
            }
        }
        try_fire() {
            if (!this.mov.on_ground)
                return;
            let w = this.get_world();
            let this_pos = this.owner.get_pos();
            let trg_pos = this.target.get_pos();
            let hit = w.sweep_aabb(this_pos, trg_pos, v2(5, 5), 2 /* geom */ | 16 /* player */);
            if (hit && hit.actor) {
                this.think_event.fire_in = 1;
                this.mov.input = v2();
                this.mov.vel.x = 0;
                let dir = qm_sub(trg_pos, this_pos);
                spawn_projectile(w, this_pos, dir, this.owner, { lifespan: 3, gravity: 0, cc: 16 /* player */ | 2 /* geom */ });
            }
        }
        take_damage(e) {
            this.think_event.fire_in = 0.5 * this.dimishing_return;
            this.mov.input = v2();
            this.mov.vel.x -= 1000 * e.dir.x * this.dimishing_return;
            this.mov.vel.y -= 300 * this.dimishing_return * (this.mov.flying ? 0.1 : 1);
            this.dimishing_return = qm_clamp(this.dimishing_return - 0.1, 0, 1);
            super.take_damage(e);
        }
    }
    function qi_spawn_slime(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), { x, y, coll_mask: 8 /* pawn */, root: true });
        r.bounds = v2(9, 6);
        r.sequences['idle'] = new qr_sprite_sequence(g_character_spritesheet, [4, 5], [.12, .12]);
        r.sequences['idle'].loop = true;
        r.play('idle');
        r.offset.y -= 3;
        qf_attach_cmp(a, new qi_ai_movement());
        qf_attach_cmp(a, new qi_slime_controller());
        return a;
    }
    function qi_spawn_humanoid(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), { x, y, coll_mask: 8 /* pawn */, root: true });
        r.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [2, 3]);
        r.sequences['walk'].loop = true;
        r.sequences['walk'].set_duration(0.15);
        r.offset.y = -5;
        r.play('walk');
        let mov = qf_attach_cmp(a, new qi_ai_movement());
        mov.ground_acc *= 0.2;
        // mov.max_velocity = 150;
        // mov.max_velocity_on_ground =  50;
        let h = qf_attach_cmp(a, new qi_humanoid_controller());
        h.hitpoints = 4;
        h.max_hitpoints = 3;
        return a;
    }
    /**
     * GLOBALS
     */
    const g_spritesheet_image = (_ => { let i = new Image(); i.src = 's.png'; return i; })();
    const g_negative_spritesheet_image = qr_create_canvas(128, 128);
    const g_character_spritesheet = new qr_spritesheet(g_spritesheet_image, g_negative_spritesheet_image, v2(14, 18), v2(9, 9));
    const g_tile_spritesheet = new qr_spritesheet(g_spritesheet_image, g_negative_spritesheet_image, v2(14, 14), v2(9, 10));
    var g_stage = 1;
    var g_scene_offset = v2();
    /**
     *  SPAWN METHODS
     */
    function spawn_bat(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), { x, y, coll_mask: 8 /* pawn */, height: 12, root: true });
        r.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [6, 7]);
        r.sequences['walk'].loop = true;
        r.sequences['walk'].set_duration(0.25);
        // r.offset.y = 0;
        r.play('walk');
        let mov = qf_attach_cmp(a, new qi_ai_movement());
        mov.gravity = 0;
        mov.flying = true;
        // mov.bounce_off_wall = true;
        mov.air_acc = 100;
        let h = qf_attach_cmp(a, new qi_humanoid_controller());
        h.hitpoints = 4;
        h.path_filter = qi_flying_path_filter;
        return a;
    }
    function spawn_coin(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_sprite_component(), { x, y, width: 8, height: 7, root: true });
        r.sprite = g_character_spritesheet.get_sprite(33);
        let m = qf_attach_cmp(a, new qc_projectile_movement());
        m.bounce_off_walls = true;
        m.bounce_factor = 0.8;
        m.rotate_root = false;
        m.collision_channel = 2 /* geom */;
        let p = qf_attach_cmp(a, new qc_pickable_component());
        p.activate_delegate = _ => a.destroy();
        return a;
    }
    function spawn_freeze_wave(world, { x, y }) {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_wave_component(), { x, y, root: true });
        return a;
    }
    function spawn_explosion(world, { x, y, radious = 15 }) {
        let a = world.spawn_actor();
        let e = qf_attach_prim(a, new qc_explosion_component(), { x, y });
        e.radious = (qm_rnd_normal() + 1) * radious;
        return a;
    }
    class player_movement extends qc_character_movement {
        process_input() {
            const speed = 900;
            this.acc.x = 0;
            this.acc.y = 0;
            let k = qs_input.keyboard;
            const move_left = k.is_down('ArrowLeft');
            const move_right = k.is_down('ArrowRight');
            const jump = k.just_pressed('ArrowUp') || k.just_pressed(' ');
            if (move_left) {
                // this.acc.x = -speed * (this.on_ground ? 1 : 0.5);
                this.acc.x = -speed;
                this.moving_left = true;
            }
            else if (move_right) {
                // this.acc.x = speed * (this.on_ground ? 1 : 0.5);
                this.acc.x = speed;
                this.moving_left = false;
            }
            else if (this.on_ground) {
                this.vel.x *= 0.4;
            }
            if (jump) {
                if (this.on_ground) {
                    this.acc.y = -speed * 100;
                }
                else if (move_left || move_right) {
                    let wall_trace = this.trace_wall(5);
                    if (wall_trace) {
                        let [p, n] = wall_trace;
                        let r = this.owner.root;
                        let dist = qm_mag(qm_sub(p, r.pos)) - r.bounds.x / 2;
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
            delta = delta * 1 / __.g_time_dilation;
            this.process_input();
            super.tick(delta);
        }
    }
    class qc_projectile_movement extends qf_component_base {
        constructor() {
            super(...arguments);
            this.acc = v2(0, 1000);
            this.vel = v2();
            this.lifespan = 0;
            this.damage = 1;
            this.rotate_root = true;
            this.bounce_off_walls = false;
            this.bounce_factor = 0.5; // 1 = no velocity loss
            this.instigator = null;
            this.collision_channel = 4294967295 /* all */;
            this.on_hit = new qf_multicast_delegate();
            this.wants_tick = true;
        }
        begin_play() {
            if (this.lifespan > 0) {
                this.get_timer().delay(this.lifespan, _ => this.owner.destroy(), this.owner);
            }
        }
        tick(delta) {
            let r = this.owner.root;
            let w = this.owner.world;
            this.vel = qm_add(this.vel, qm_scale(this.acc, delta));
            let ds = qm_scale(this.vel, delta);
            let end = qm_add(r.pos, ds);
            let hit_result = w.sweep_aabb(r.pos, end, r.bounds, this.collision_channel);
            if (hit_result) {
                if (this.bounce_off_walls) {
                    if (hit_result.normal.x != 0)
                        this.vel.x *= -this.bounce_factor;
                    if (hit_result.normal.y != 0)
                        this.vel.y *= -this.bounce_factor;
                }
                else {
                    this.vel = v2();
                    this.acc = v2();
                    this.wants_tick = false;
                }
                r.pos = v2c(hit_result.pos);
                this.on_hit.broadcast(hit_result, this);
            }
            else {
                r.pos = end;
                if (this.rotate_root) {
                    r.rot = Math.atan2(this.vel.y, this.vel.x);
                }
            }
        }
    }
    // #DEBUG-BEGIN
    __.g_draw_considered = false;
    __.g_draw_blocking_dist = false;
    __.g_draw_floor_dist = false;
    __.g_draw_jump_dist = false;
    __.g_draw_id = false;
    __.g_draw_bounds = false;
    __.g_draw_paths = false;
    class debug_draw_collisions extends qf_scene_component {
        begin_play() {
            this.tiles = this.owner.world.geometry;
        }
        render_c2d_impl(ctx) {
            const tl = this.tiles.tile_size;
            ctx.strokeStyle = 'white';
            ctx.fillStyle = 'red';
            // let to_local = this.get_world_transform();
            // mat_invert(to_local);
            // const pos = transform(this.query_start.root.pos, to_local)
            // const end = transform(input.mouse.pos, to_local);
            for (let x = 0; x < this.tiles.width; ++x) {
                for (let y = 0; y < this.tiles.height; ++y) {
                    if (__.g_draw_bounds) {
                        if (this.tiles.is_blocking(x, y)) {
                            ctx.strokeRect(x * tl, y * tl, tl, tl);
                            // this.tiles.line_trace_tile(pos, end, x, y);
                        }
                    }
                    if (__.g_draw_blocking_dist) {
                        ctx.fillStyle = 'red';
                        ctx.fillText(this.tiles.blocking_dist[y][x].toFixed(0), x * tl, (y + 1) * tl);
                    }
                    if (__.g_draw_floor_dist) {
                        ctx.fillStyle = 'green';
                        ctx.fillText(this.tiles.floor_dist[y][x].toFixed(0), (x + 0.5) * tl, (y + 1) * tl);
                    }
                    if (__.g_draw_jump_dist) {
                        ctx.fillStyle = 'pink';
                        ctx.fillText(this.tiles.jump_dist[y][x].toFixed(0), (x) * tl, (y + 0.5) * tl);
                    }
                    if (__.g_draw_id) {
                        ctx.fillStyle = 'red';
                        ctx.fillText(x.toFixed(0), (x) * tl, (y + 0.5) * tl);
                        ctx.fillText(y.toFixed(0), (x + 0.5) * tl, (y + 1) * tl);
                    }
                }
            }
            if (__.g_draw_bounds) {
                ctx.strokeStyle = 'pink';
                for (let act of this.owner.world.actors) {
                    for (let p of act.getcmp(qf_scene_component)) {
                        let h = qm_scale(p.bounds, 0.5);
                        ctx.strokeRect(p.pos.x - h.x, p.pos.y - h.y, h.x * 2, h.y * 2);
                    }
                }
            }
            ctx.strokeStyle = 'green';
            for (let path of qi_g_paths) {
                if (path.length == 0)
                    continue;
                ctx.beginPath();
                ctx.moveTo(path[0].x, path[0].y);
                for (let node of path) {
                    ctx.lineTo(node.x, node.y);
                }
                ctx.stroke();
            }
            qi_g_paths = [];
            // ctx.beginPath();
            // ctx.moveTo(pos.x, pos.y);
            // ctx.lineTo(end.x, end.y);
            // ctx.stroke();
            let considered = this.tiles.d_considered;
            //let trace_result = this.tiles.line_trace2(pos, end);
            //this.tiles.sweep_aabb(pos, end, this.query_start.root.bounds);
            ctx.strokeStyle = 'green';
            ctx.lineWidth = 0.5;
            if (__.g_draw_considered)
                for (let r of considered)
                    ctx.strokeRect(r.x, r.y, tl, tl);
            // let hits = this.tiles.d_hits;
            // for (let [p, n] of hits) ctx.fillRect(p.x - 2, p.y - 2, 4, 4);
            this.tiles.d_considered = [];
            this.tiles.d_hits = [];
        }
    }
    // #DEBUG-END
    function spawn_projectile(world, loc, dir, instigator, { lifespan = 0, gravity = 1000, cc = 4294967295 /* all */ }) {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), { root: true });
        r.pos = loc;
        r.bounds = v2(8, 6);
        r.sequences['fire'] = new qr_sprite_sequence(g_character_spritesheet, [27, 28], [0.02, 1]);
        r.sequences['explode'] = new qr_sprite_sequence(g_character_spritesheet, [29, 30, qm_rnd_select(31, 32), 8], [0.03, 0.03, 0.05, 1]);
        r.play('fire');
        let p = new qc_projectile_movement();
        p.vel = dir;
        p.acc.y = gravity;
        p.lifespan = lifespan;
        p.on_hit.bind(_ => r.play('explode'), r);
        p.instigator = instigator;
        p.collision_channel = 4294967295 /* all */;
        p.on_hit.bind(hit => {
            if (hit.actor) {
                hit.actor.on_take_damage.broadcast(new qf_damage_event(p.damage, p.instigator, hit.normal));
            }
            if (qm_rnd() > 0.8)
                spawn_explosion(p.get_world(), hit.pos);
        }, a);
        qf_attach_cmp(a, p);
        return a;
    }
    class player_controller extends qf_component_base {
        constructor() {
            super(...arguments);
            this.bullet_speed = 300;
            this.fire_spread = 5;
            this.bullets_per_shoot = 1;
            this.wants_tick = true;
            this.fire_rate = 4;
        }
        begin_play() {
            [this.movement] = this.owner.getcmp(player_movement);
            [this.sprite] = this.owner.getcmp(qc_anim_sprite_component);
            [this.weapon_sprite] = this.owner.getcmp_byname('weapon_sprite');
            this.owner.on_take_damage.bind(this.take_damage, this);
            this.set_fire_rate(this.fire_rate);
            let counter = 0;
            qi_g_on_enemy_killed.bind(a => {
                counter += 1;
                this.set_fire_rate(4 + counter / 4);
                this.bullet_speed = qm_clamp(this.bullet_speed + 10, 10, 1000);
                if (this.bullets_per_shoot < 10 && counter % 5 == 0) {
                    this.bullets_per_shoot += 1;
                }
            }, this);
        }
        tick(delta) {
            let a = this.owner;
            this.sprite.flip_x = !this.movement.moving_left;
            this.weapon_sprite.pos.x = this.movement.moving_left ? -10 : 10;
            this.weapon_sprite.flip_x = !this.movement.moving_left;
            this.weapon_sprite.rot *= 0.9;
            let vel = this.movement.vel;
            if (this.movement.on_ground) {
                if (qm_eq_eps(vel.x, 0, 3)) {
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
            if (qs_input.keyboard.is_down('z')) {
                this.fire_delegate();
            }
            // this.getworld().timer.throttle(0.1, (i: number) => i, this);
            // let z_state = input.keyboard.get_state('z');
            // if (z_state.is_down)
            // {
            //     this.z_press_start = input.keyboard.get_state('z').timestamp;
            // }
            // if (this.z_press_start > 0)
            // {
            //     const press_time = Date.now() - this.z_press_start;
            //     const can_fire = this.last_fire_time - this.z_press_start != 0;
            //     if (can_fire && (!z_state.is_down|| press_time > 1000)) {
            //         this.last_fire_time = this.z_press_start;
            //         this.z_press_start = 0;
            //         let ampl = clamp(press_time / 100, 1, 10);
            //         let dir = clamp_mag(add(
            //                 scale(this.movement.moving_left ? left : right, 100 * ampl),
            //                 v(0, 0)), 0, 800);
            //         spawn_projectile(a.world, a.root.pos, dir, {lifespan: 0.6, gravity: 0});
            //     }
            // }
        }
        take_damage(e) {
            this.movement.vel.x += e.dir.x * 100;
            this.movement.vel.y -= 100;
            // this.sprite.blink();
            spawn_freeze_wave(this.get_world(), this.owner.get_pos());
            // reset();
        }
        set_fire_rate(rate) {
            this.fire_delegate = this.get_timer().throttle(1 / qm_clamp(rate, 0.5, 1000), this.fire, this);
        }
        fire() {
            let a = this.owner;
            let ml = this.movement.moving_left;
            let dir = ml ? qm_left : qm_right;
            let angle = qm_clamp((this.bullets_per_shoot - 1) * 0.087, 0, 0.785) * dir.x;
            let step = 0;
            if (this.bullets_per_shoot > 1) {
                step = (2 * angle) / (this.bullets_per_shoot - 1);
            }
            dir = qm_rotate(dir, -angle);
            for (let i = 0; i < this.bullets_per_shoot; ++i) {
                spawn_projectile(a.world, this.weapon_sprite.get_pos(), qm_scale(dir, this.bullet_speed), this.owner, { lifespan: 0.3, gravity: 0, cc: 8 /* pawn */ | 2 /* geom */ });
                dir = qm_rotate(dir, step);
            }
            if (this.movement.on_ground)
                this.movement.vel.x += dir.x * -0.1;
            this.weapon_sprite.rot = this.movement.moving_left ? 3.14 / 2 : -3.14 / 2;
        }
    }
    // class buff_spawner extends component_base {
    //     protected enabled = false;
    //     public begin_play(): void {
    //         g_on_enemy_killed.bind(this.on_enemy_killed, this);
    //     }
    //     public tick(delta: number) {
    //         if (!this.enabled) return;
    //         if (overlap_aabb(this.owner.root.get_aabb(), this.get_world().player.root.get_aabb())) {
    //         }
    //     }
    //     protected on_enemy_killed(): void {
    //     }
    // }
    function spawn_player(world, { x, y }) {
        let a = world.spawn_actor();
        let s = new qc_anim_sprite_component();
        let idle = s.sequences['idle'] = new qr_sprite_sequence(g_character_spritesheet, [0, 1]);
        idle.loop = true;
        s.sequences['jump'] = new qr_sprite_sequence(g_character_spritesheet, [9]);
        s.sequences['falling'] = new qr_sprite_sequence(g_character_spritesheet, [0]);
        let walk = s.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [9, 10, 11, 10]);
        walk.loop = true;
        walk.set_duration(0.3);
        s.play('walk');
        s.offset = v2(0, -1);
        qf_attach_prim(a, s, { x, y, width: 10, height: 14, coll_mask: 16 /* player */, root: true });
        let pistol = qf_attach_prim(a, new qc_sprite_component(), { x: 10, y: 0, name: 'weapon_sprite' });
        pistol.parent = s;
        pistol.flip_x = true;
        pistol.sprite = g_character_spritesheet.get_sprite(18);
        qf_attach_cmp(a, new player_movement());
        qf_attach_cmp(a, new player_controller());
        // #DEBUG-BEGIN
        let tdebug = new debug_draw_collisions();
        qf_attach_prim(a, tdebug, {});
        // #DEBUG-END
        world.player = a;
        return a;
    }
    function spawn_tile(actor, { x, y }, id) {
        let s = qf_attach_prim(actor, new qc_sprite_component(), { x, y, width: 10, height: 10 });
        s.sprite = g_tile_spritesheet.get_sprite(9 * 8 + parseInt(id, 16));
    }
    function parse_level(data, world) {
        let geom = world.geometry;
        let x = 0, y = 0;
        let tile_map = world.spawn_actor();
        for (let line of data.split('\n')) {
            x = 0;
            for (let char of line) {
                let pos = qm_scale(v2(x + 0.5, y + 0.5), geom.tile_size);
                if (char === '0' || char === '1') {
                    geom.set_blocking(x, y, true);
                    spawn_tile(tile_map, pos, char);
                }
                if (char === '@')
                    spawn_player(world, pos);
                if (char === 's')
                    qi_spawn_slime(world, pos);
                if (char === 'h')
                    qi_spawn_humanoid(world, pos);
                x += 1;
            }
            y += 1;
        }
    }
    let world;
    let ctx;
    __.g_time_dilation = 1;
    function tick() {
        world.tick(0.016 * __.g_time_dilation);
        ctx.save();
        ctx.clearRect(0, 0, 410, 210);
        ctx.translate(g_scene_offset.x, g_scene_offset.y);
        qr_render_w(ctx, world);
        ctx.restore();
        window.requestAnimationFrame(tick);
    }
    function reset() {
        if (world) {
            console.log('score: ', g_stage);
        }
        g_stage = 1;
        let t = new qf_tile_geometry(28, 14, 14);
        world = new qf_world();
        world.geometry = t;
        parse_level(`0000000000000000000000000000
1                          0
1                          0
1               s          0
1               11         0
1                          0
1                          0
1        @                 0
1      000000     000000   0
1      000                 0
1      00                  0
1      0                   0          
1                          0
0000000000000000000111111111`, world);
        world.has_begun_play = true;
        for (let a of world.actors)
            for (let c of a.components)
                c.begin_play();
    }
    function main() {
        let canvas = document.querySelector("#canvas");
        ctx = canvas.getContext("2d");
        ctx['imageSmoothingEnabled'] = false;
        ctx.scale(2, 2);
        ctx.translate(5.5, 5.5);
        {
            let c = g_negative_spritesheet_image.getContext('2d');
            let { width, height } = g_negative_spritesheet_image;
            c.drawImage(g_spritesheet_image, 0, 0);
            c.fillStyle = 'white';
            c.globalCompositeOperation = 'source-in';
            c.fillRect(0, 0, width, height);
            // c.globalCompositeOperation = 'difference';
            // c.drawImage(g_spritesheet_image, 0, 0);
        }
        reset();
        qs_input.init(canvas);
        window.requestAnimationFrame(tick);
    }
    function load() {
        if (qr_is_img_loaded(g_spritesheet_image)) {
            main();
        }
        else {
            setTimeout(load, 100);
        }
    }
    load();
})(__ || (__ = {}));
