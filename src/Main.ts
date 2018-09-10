namespace g {
// math
    class qm_vec { 
        constructor(
            public x: number,
            public y: number
        ) { }
    }
    
    class qm_cvec {
        constructor(
            public readonly x: number,
            public readonly y: number
        ) { }
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
    function v2c(a: qm_vec) { return new qm_vec(a.x, a.y); }
    function qm_eq(a: qm_vec, b: qm_vec) { return a.x == b.x && a.y == b.y; }
    function qm_add(a: qm_vec, b: qm_vec) { return v2(a.x + b.x, a.y + b.y); }
    function qm_sub(a: qm_vec, b: qm_vec) { return v2(a.x - b.x, a.y - b.y); }
    function qm_mul(a: qm_vec, b: qm_vec) { return v2(a.x * b.x, a.y * b.y); }
    function qm_dot(a: qm_vec, b: qm_vec) { return a.x * b.x + a.y * b.y; }
    function qm_scale(a: qm_vec, s: number) { return v2(a.x * s, a.y * s); }
    function qm_cross(a: qm_vec, b: qm_vec) { return a.x * b.y - a.y * b.x; }
    function qm_sign(a: qm_vec) { return v2(Math.sign(a.x), Math.sign(a.y)); }
    function qm_mag_sqr(a: qm_vec) { return a.x * a.x + a.y * a.y; }
    function qm_mag(a: qm_vec) { return Math.sqrt(qm_mag_sqr(a)); }
    function qm_unit(a: qm_vec) { let m = qm_mag(a); return qm_scale(a, 1/m); }
    function qm_clamp_mag(a: qm_vec, min: number, max: number = min) {
        let m = qm_mag(a);
        return m < min ? qm_scale(qm_unit(a), min) :
               m > max ? qm_scale(qm_unit(a), max) :
                         a;
    }
    function qm_rotate(a: qm_vec, rad: number): qm_vec {
        let c = Math.cos(rad);
        let s = Math.sin(rad);
        return v2(c * a.x - s * a.y, s * a.x + c * a.y);
    }
    function qm_manhattan_dist(a: qm_vec) { return Math.abs(a.x) + Math.abs(a.y); }

    // general math
    function qm_clamp(x: number, min: number, max: number) { return Math.max(Math.min(x, max), min); }
    function qm_eq_eps(a: number, b: number, eps = 0.01) { return Math.abs(a - b) < eps; }
    function qm_rnd(min = 0, max = 1) { return min + (max - min) * Math.random(); }
    function qm_rnd_select<T>(...elements: T[]): T { return elements[qm_rnd(0, elements.length) | 0]};
    // The Box-Muller transform converts two independent uniform variates on (0, 1) into two standard Gaussian variates (mean 0, variance 1). 
    function qm_rnd_normal() {
        let u = 0, v = 0;
        while (u === 0) u = Math.random(); //Converting [0,1) to (0,1)
        while (v === 0) v = Math.random();
        let n = Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
        return qm_clamp(n/10, -1, 1);
    }

    type qm_aabb = [qm_vec, qm_vec];

    // a, b - any two points
    // return [center, half_size]
    function qm_make_aabb(a: qm_vec, b: qm_vec): qm_aabb
    {
        let tl = v2(Math.min(a.x, b.x), Math.min(a.y, b.y));
        let br = v2(Math.max(a.x, b.x), Math.max(a.y, b.y));

        return [qm_scale(qm_add(tl, br), 0.5), qm_scale(qm_sub(br, tl), 0.5)];
    }

    function qm_overlap_point(a: qm_vec, [c, ext]: qm_aabb): boolean {
        let d = qm_sub(a, c);
        return Math.abs(d.x) < ext.x && Math.abs(d.y) < ext.y;
    }
    function qm_overlap_aabb([ac, aext]: qm_aabb, [bc, bext]: qm_aabb): boolean {
        return qm_overlap_point(ac, [bc, qm_add(aext, bext)]);
    }

    // position, normal
    type qm_line_trace_result = [qm_vec, qm_vec];

    function qm_line_trace_aabb(start: qm_vec, end: qm_vec, aabb: qm_aabb): qm_line_trace_result
    {
        let get_vertex = (i: number) => {
            let [center, ext] = aabb;
            const hx = [-ext.x, ext.x, ext.x, -ext.x];
            const hy = [-ext.y, -ext.y, ext.y, ext.y];
            return v2(center.x + hx[i % 4], center.y + hy[i % 4]);
        };

        const trace = qm_sub(end, start);
        let hits: qm_vec[] = new Array(4);

        for (let i = 0; i < 4; ++i)
        {
            let v1 = get_vertex(i), 
                v2 = get_vertex(i + 1);
            let edge = qm_sub(v2, v1);

            // if trace start point is above edge
            if (qm_cross(edge, qm_sub(start, v1)) <= 0 && 
            // and end trace is below line
                qm_cross(edge, qm_sub(end, v1)) >= 0)
            {
                // trace is vertical line
                if (trace.x == 0)
                {
                    // we can only colide against horizontal line
                    if (i == 0 || i == 2)
                    {
                        if (start.x > Math.min(v1.x, v2.x) && start.x < Math.max(v1.x, v2.x))
                        {
                            hits[i] = new qm_vec(start.x, v1.y);
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
                    
                    if (x > Math.min(v1.x, v2.x) && x < Math.max(v1.x, v2.x))
                    {
                        hits[i] = new qm_vec(x, y);
                    }
                }
                // vertical line
                else 
                {
                    let x = v1.x;
                    let y = a * x + b;

                    if (y > Math.min(v1.y, v2.y) && y < Math.max(v1.y, v2.y))
                    {
                        hits[i] = new qm_vec(x, y);
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
                let dist = qm_mag_sqr(qm_sub(hits[i], start));
                if (dist < best_dist)
                {
                    best_dist = dist;
                    best_index = i;
                }
            }
        }

        if (best_index != -1)
        {
            const normal = [qm_up, qm_right, qm_down, qm_left];
            return [hits[best_index], normal[best_index]];
        }
    }

    type mat = Float32Array;

    function qm_mat(m00 = 1, m10 = 0, m01 = 0, m11 = 1, tx = 0, ty = 0): mat
    {
        return new Float32Array([m00, m10, 0, m01, m11, 0, tx, ty, 1]);
    }

    function qm_transform(v: qm_vec, m: mat): qm_vec
    {
        return v2(v.x * m[0] + v.y * m[3] + m[6], 
                    v.x * m[1] + v.y * m[4] + m[7]);
    }

    function qm_mat_mul( a: mat, b: mat, out: mat ): void
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
    function qm_mat_invert( a: mat, out = a ): mat
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

// utilites
    function qu_has_method<T, I>(obj: T, name: keyof I ): obj is T & I
    {
        return typeof obj[<any>name] == 'function';
    }

    function qu_log(...msg: any[]): void {
        // #DEBUG-BEGIN
        console.log(...msg);
        // #DEBUG-END
    }

    function qu_assert(cond: boolean, msg = `Assert failed`) {
        // #DEBUG-BEGIN
        if (!cond) throw new Error(msg);
        // #DEBUG-END
    }

    function qu_contains<T>(arr: T[], elem: T): boolean {
        return arr.indexOf(elem) >= 0;
    }

    function qu_greatest_element<T>(arr: T[], cmp: (a: T, b: T) => boolean): T {
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

// framework
    type qf_bindable = qf_actor | qf_component_base;

    class qf_multicast_delegate<T extends Function> {
        protected delegates: [T, qf_bindable][] = [];

        public broadcast(...args: any[]): void {
            for (let i = this.delegates.length - 1; i >= 0; --i) {
                let [d, owner] = this.delegates[i];
                if (owner.is_valid()) {
                    d.apply(owner, args);
                } else {
                    this.delegates.splice(i, 1);
                }
            }
        }

        public bind(fn: T, owner: qf_bindable): void {
            if (owner.is_valid()) {
                this.delegates.push([fn, owner]);
            }
        }
    }

    class qf_hit_result {
        constructor(
            public readonly pos: qm_vec = qm_zero,
            public readonly normal: qm_vec = qm_up,
            public readonly actor: qf_actor = undefined
        ) { }
    }

    // collision channgel
    const enum qf_cc {
        none      = 0,
        geom      = 1 << 1,
        // visibilty = 1 << 2,
        enemy      = 1 << 3,
        player    = 1 << 4,
        all       = 0xffffffff
    }

    function qf_has_tick_method<T extends qf_component_base>(obj: T): obj is (T & {tick: (d:number) => void}) {
        return obj.wants_tick;
    }

    abstract class qf_component_base
    {
        public owner: qf_actor;
        public name = '';
        public wants_tick = false;
        public received_begin_play = false;

        public begin_play(): void { }
        public get_world() { return this.owner.world; }
        public get_timer() { return this.owner.world.timer; }
        public is_valid() { return this.owner && this.owner.is_valid(); }
    }

    abstract class qf_scene_component extends qf_component_base
    {
        public pos: qm_vec = v2(0, 0);
        public scale: qm_vec = v2(1, 1);
        public rot: number = 0;
        public parent: qf_scene_component;
        public bounds: qm_vec = v2(0, 0);
        public visible = true;
        public collision_mask = qf_cc.none;

        public render_c2d(ctx: CanvasRenderingContext2D): void
        {
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

        public get_pos(): qm_vec {
            if (this.parent) {
                return qm_transform(qm_zero, this.get_world_transform());
            }
            return v2c(this.pos);
        }

        public get_aabb(ext: qm_vec = qm_zero): qm_aabb {
            return [v2c(this.pos), qm_add(qm_scale(qm_mul(this.bounds, this.scale), 0.5), ext)];
        }

        public get_local_transform(): mat {
            return qm_mat(this.scale.x, 0, 0, this.scale.y, this.pos.x, this.pos.y);
        }

        public get_world_transform(): mat {
            let base = this.parent ? this.parent.get_world_transform() : qm_mat();
            qm_mat_mul(base, this.get_local_transform(), base);
            return base;
        }

        public abstract render_c2d_impl(ctx: CanvasRenderingContext2D): void;
    }

    class qf_damage_event {
        constructor(
            public readonly damage: number,
            public readonly instigator: qf_actor,
            public readonly dir = v2()
        ) { }
    }

    type qf_damage_delegate = (e: qf_damage_event) => void;

    class qf_actor
    {
        public world: qf_world;
        public components: qf_component_base[] = [];
        public root: qf_scene_component;
        public readonly on_take_damage = new qf_multicast_delegate<qf_damage_delegate>();

        public is_valid(): boolean {
            return this.world && this.world === g_world;
        }

        public destroy() { 
            this.world.destroy_actor(this); 
        }

        public get_pos(): qm_vec {
            return this.root.get_pos();
        }
        
        public getcmp<T extends qf_component_base>(clazz: Function & { prototype: T }): T[] {
            return this.components.filter(c => c instanceof clazz) as T[];
        }

        public getcmp_byname(name: string): qf_component_base[] {
            return this.components.filter(c => c.name === name);
        }
    }

    function qf_attach_cmp<T extends qf_component_base>(owner: qf_actor, cmp: T, name = ''): T
    {
        cmp.owner = owner;
        cmp.name = name;
        owner.components.push(cmp);
        return cmp;
    }

    function qf_attach_prim<T extends qf_scene_component>(owner: qf_actor, prim: T, 
        {x = 0, y = 0, width = 10, height = 10, root = false, coll_mask = qf_cc.none, name = ''}): T
    {
        prim.pos.x = x;
        prim.pos.y = y;
        prim.bounds.x = width;
        prim.bounds.y = height;
        prim.collision_mask = coll_mask;

        if (root) owner.root = prim;
        return qf_attach_cmp(owner, prim, name);
    }

    class qf_world
    {
        public actors: qf_actor[] = [];
        public geometry: qf_tile_geometry;
        public player: qf_actor;
        public timer = new qf_timer();

        public tick(delta: number)
        {
            this.timer.tick(delta);

            for (let actor of this.actors) {
                for (let cmp of actor.components) {
                    if (!cmp.received_begin_play) {
                        cmp.begin_play();
                        cmp.received_begin_play = true;
                    }
                    if (qf_has_tick_method(cmp)) {
                        cmp.tick(delta);
                    }
                }
            }
        }

        public spawn_actor(): qf_actor
        {
            let a = new qf_actor();
            a.world = this;
            this.actors.push(a);
            return a;
        }

        public destroy_actor(actor: qf_actor): void
        {
            let i = this.actors.indexOf(actor);
            this.actors.splice(i, 1);
            actor.world = undefined;
        }

        public overlap(aabb: qm_aabb, channel = qf_cc.all): qf_actor[] {
            return this.actors.filter(a => {
                return a.root && 
                       (a.root.collision_mask & channel) &&
                       qm_overlap_aabb(a.root.get_aabb(), aabb);
            });
        }

        public sweep_aabb(start: qm_vec, end: qm_vec, size: qm_vec, channel = qf_cc.all, ignore?: qf_actor[]): qf_hit_result {
            const area = qm_make_aabb(start, end);
            const half_size = qm_scale(size, 0.5);

            let hits: qf_hit_result[] = [];

            if (channel & qf_cc.geom) {
                let hit = this.geometry.sweep_aabb(start, end, size);
                if (hit) {
                    hits.push(new qf_hit_result(hit[0], hit[1]));
                }
            }

            if (channel === qf_cc.geom) {
                return hits[0];
            }

            for (let actor of this.actors) {
                if (ignore && qu_contains(ignore, actor)) {
                    continue;
                }
                if (actor.root) {
                    let actor_aabb = actor.root.get_aabb(half_size);
                    if ((actor.root.collision_mask & channel) && qm_overlap_aabb(area, actor_aabb)) {
                        let hit = qm_line_trace_aabb(start, end, actor_aabb);
                        if (hit) {
                            hits.push(new qf_hit_result(hit[0], hit[1], actor));
                        } else {
                            hits.push(new qf_hit_result(start, qm_unit(qm_sub(start, end)), actor));
                        }
                    }
                }
            }

            return qu_greatest_element(hits, (a, b) => {
                return qm_mag_sqr(qm_sub(a.pos, start)) > qm_mag_sqr(qm_sub(b.pos, start));
            });
        }

        
    }

    function qf_easing_sin_inout(t: number) {
        return (1 - Math.cos(Math.PI * t)) / 2;
    }

    // class qf_tween {
    //     protected next: qf_tween;
    //     protected prev: qf_tween;
    //     protected total_elapsed = 0;
    //     protected do_loop = false;

    //     constructor(
    //         readonly obj: Object,
    //         readonly prop: string | symbol | number,
    //         readonly start_value: number,
    //         readonly end_value: number,
    //         readonly duration: number,
    //         readonly easing: (a: number) => number,
    //     ) {
    //         qu_assert(!!obj);
    //         qu_assert(typeof obj[prop] === 'number');
    //         qu_assert(duration > 0);
    //     }

    //     public tick(delta: number): void {
    //         this.total_elapsed += delta;
    //         let e = this.total_elapsed;

    //         for (let self: qf_tween = this; self; self = self.next) {
    //             if (e < self.duration) {
    //                 let a = qm_clamp(e / self.duration, 0, 1);
    //                 self.obj[self.prop] = self.start_value + (self.end_value - self.start_value) * self.easing(a);
    //                 break;
    //             } else {
    //                 self.obj[self.prop] = self.end_value;
    //                 e -= self.duration;

    //                 if (self.do_loop) {
    //                     this.total_elapsed = 0;
    //                     break;
    //                 }
    //             }
    //         }
    //     }

    //     public then<T, K extends keyof T>(obj: T, prop: K, start_value: number, end_value: number, duration: number, easing = qf_easing_sin_inout): qf_tween {
    //         let t = new qf_tween(obj, prop, start_value, end_value, duration, easing);
    //         t.prev = this;
    //         this.next = t;
    //         return t;
    //     }

    //     public loop(): qf_tween { this.do_loop = true; return this.get_head(); }

    //     public get_head() {
    //         for (let s: qf_tween = this; s; s = s.prev) {
    //             if (!s.prev) return s;
    //         }
    //     }
    //     public is_tail() { return !this.next; }
    //     public is_head() { return !this.prev; }

    //     public total_duration(): number {
    //         let dur = 0;
    //         for (let self: qf_tween = this; self; self = self.next) {
    //             dur += self.duration;
    //         }
    //         return dur;
    //     }

    //     public is_done(): boolean {
    //         return this.total_elapsed / this.total_duration() >= 1;
    //     }
    // }

    const enum qf_timer_type
    {
        once,
        repeat
    }

    class qf_timer_event {
        public fire_in = 0;

        constructor(
            public owner: qf_timer,
            public type: qf_timer_type,
            public delay: number,
            public fn: Function,
            public actor: qf_actor,
            public ctx?: any
        ) { 
            this.fire_in = delay;
        }

        public execute() {
            if (this.actor && this.actor.is_valid()) {
                this.fn.call(this.ctx);
            }
        }

        public is_valid() {
            return this.actor && this.actor.is_valid() &&
                   (this.type === qf_timer_type.repeat || (this.type === qf_timer_type.once && this.fire_in >= 0));
        }

        public invalidate() {
            this.actor = undefined;
        }

        public reset() { 
            this.fire_in += this.delay; 
        }
    }

    class qf_timer
    {
        public events: qf_timer_event[] = [];

        public tick(delta: number)
        {
            for (let i = this.events.length - 1; i >= 0; --i)
            {
                let e = this.events[i];

                if (!e.is_valid()) {
                    this.events.splice(i, 1);
                    continue;
                }

                e.fire_in -= delta;
                if (e.fire_in <= 0) {
                    e.execute();
                    if (e.type == qf_timer_type.repeat) e.reset();
                }
            }
        }

        protected add_timer(time: number, type: qf_timer_type, fn: Function, ctx: qf_actor | qf_component_base): qf_timer_event {
            let actor: qf_actor;

            if (ctx instanceof qf_actor) {
                actor = ctx;
            } else {
                actor = ctx.owner;
            } 
        
            let e = new qf_timer_event(this, type, time, fn, actor, ctx);
            this.events.push(e);
            return e;
        }

        public delay(delay: number, fn: Function, ctx: qf_actor | qf_component_base): qf_timer_event
        {
            return this.add_timer(delay, qf_timer_type.once, fn, ctx);
        }

        public every(timespan: number, fn: Function, ctx: qf_actor | qf_component_base): qf_timer_event
        {
            return this.add_timer(timespan, qf_timer_type.repeat, fn, ctx);
        }

        public throttle<T extends (...args: any[]) => void>(delay: number, fn: T, ctx: qf_actor | qf_component_base): T {
            let lock = false;
            return ((...args: any[]) => {
                if (!lock) {
                    lock = true;
                    this.delay(delay, _ => lock = false, ctx);
                    fn.apply(ctx, args);
                }
            }) as T;
        }
    }

// render
	let qr_pixel_font_data = {
		
		char: {
			width: 3,
			height: 5
		},
	
		letters: {
			
			'+': [
				0, 0, 0,
				0, 1, 0,
				1, 1, 1,
				0, 1, 0,
				0, 0, 0
			],
			
			'-': [
				0, 0, 0,
				0, 0, 0,
				1, 1, 1,
				0, 0, 0,
				0, 0, 0
			],
			
			'.': [
				0, 0, 0,
				0, 0, 0,
				0, 0, 0,
				0, 0, 0,
				0, 1, 0
			],
			
			',': [
				0, 0, 0,
				0, 0, 0,
				0, 0, 0,
				0, 1, 0,
				1, 0, 0
			],
			
			':': [
				0, 0, 0,
				0, 1, 0,
				0, 0, 0,
				0, 1, 0,
				0, 0, 0
			],
			
			'?': [
				1, 1, 0,
				0, 0, 1,
				0, 1, 1,
				0, 0, 0,
				0, 1, 0
			],
			
			'♥': [
				1, 0, 1,
				1, 1, 1,
				1, 1, 1,
				0, 1, 0,
				0, 0, 0
			],
			
			
			'0': [
				1, 1, 1,
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 1, 1
			],
			
			'1': [
				0, 1, 0,
				1, 1, 0,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0
			],
			
			'2': [
				1, 1, 1,
				0, 0, 1,
				1, 1, 1,
				1, 0, 0,
				1, 1, 1
			],
			
			'3': [
				1, 1, 1,
				0, 0, 1,
				0, 1, 1,
				0, 0, 1,
				1, 1, 1
			],
			
			'4': [
				1, 0, 1,
				1, 0, 1,
				1, 1, 1,
				0, 0, 1,
				0, 0, 1
			],
			
			'5': [
				1, 1, 1,
				1, 0, 0,
				1, 1, 1,
				0, 0, 1,
				1, 1, 0
			],
			
			'6': [
				0, 1, 1,
				1, 0, 0,
				1, 1, 1,
				1, 0, 1,
				1, 1, 1
			],
			
			'7': [
				1, 1, 1,
				0, 0, 1,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0
			],
			
			'8': [
				1, 1, 1,
				1, 0, 1,
				1, 1, 1,
				1, 0, 1,
				1, 1, 1
			],
			
			'9': [
				1, 1, 1,
				1, 0, 1,
				1, 1, 1,
				0, 0, 1,
				1, 1, 0
			],
			
			'A': [
				0, 1, 0,
				1, 0, 1,
				1, 1, 1,
				1, 0, 1,
				1, 0, 1
			],
			
			'B': [
				1, 1, 0,
				1, 0, 1,
				1, 1, 0,
				1, 0, 1,
				1, 1, 0
			],
			
			'C': [
				1, 1, 1,
				1, 0, 0,
				1, 0, 0,
				1, 0, 0,
				1, 1, 1
			],
			
			'D': [
				1, 1, 0,
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 1, 0
			],
			
			'E': [
				1, 1, 1,
				1, 0, 0,
				1, 1, 0,
				1, 0, 0,
				1, 1, 1
			],
			
			'F': [
				1, 1, 1,
				1, 0, 0,
				1, 1, 0,
				1, 0, 0,
				1, 0, 0
			],
			
			'G': [
				1, 1, 1,
				1, 0, 0,
				1, 0, 1,
				1, 0, 1,
				1, 1, 1
			],
			
			'H': [
				1, 0, 1,
				1, 0, 1,
				1, 1, 1,
				1, 0, 1,
				1, 0, 1
			],
			
			'I': [
				0, 1, 0,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0
			],
			
			'J': [
				1, 1, 1,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0,
				1, 0, 0
			],
			
			'K': [
				1, 0, 1,
				1, 0, 1,
				1, 1, 0,
				1, 0, 1,
				1, 0, 1
			],
			
			'L': [
				1, 0, 0,
				1, 0, 0,
				1, 0, 0,
				1, 0, 0,
				1, 1, 1
			],
			
			
			'N': [
				0, 0, 1,
				1, 0, 1,
				1, 1, 1,
				1, 0, 1,
				1, 0, 0
			],
			
			'M': [
				1, 0, 1,
				1, 1, 1,
				1, 0, 1,
				1, 0, 1,
				1, 0, 1
			],
			
			'O': [
				1, 1, 1,
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 1, 1
			],
			
			'P': [
				1, 1, 1,
				1, 0, 1,
				1, 1, 1,
				1, 0, 0,
				1, 0, 0
			],
			
			'Q': [
				1, 1, 1,
				1, 0, 1,
				1, 0, 1,
				1, 1, 0,
				0, 0, 1
			],
			
			'R': [
				1, 1, 0,
				1, 0, 1,
				1, 1, 0,
				1, 0, 1,
				1, 0, 1
			],
			
			'S': [
				1, 1, 1,
				1, 0, 0,
				1, 1, 0,
				0, 0, 1,
				1, 1, 1
			],
			
			'T': [
				1, 1, 1,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0,
				0, 1, 0
			],
			
			'U': [
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 1, 1
			],
			
			'W': [
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 1, 1,
				1, 0, 1
			],
			
			'V': [
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				0, 1, 0
			],
			
			'X': [
				1, 0, 1,
				1, 0, 1,
				0, 1, 0,
				1, 0, 1,
				1, 0, 1
			],
			
			'Y': [
				1, 0, 1,
				1, 0, 1,
				1, 0, 1,
				0, 1, 0,
				0, 1, 0
            ],
            
            'Z': [
				1, 1, 1,
				0, 0, 1,
				0, 1, 0,
				1, 0, 0,
				1, 1, 1
			]
		},
    };

    class qr_font_bitmap {
        readonly bitmap: HTMLCanvasElement;
        readonly char_offset: {[letter:string]: number} = {};
        readonly char_width: number;
        readonly char_height: number;

        constructor() {
            let f = qr_pixel_font_data;
            let w = this.char_width = f.char.width + 2;
            let h = this.char_height = f.char.height + 2;
            let n = Object.keys(f.letters).length;

            let render_letter = (ctx: CanvasRenderingContext2D, letter: string) => {
                let w = f.char.width, h = f.char.height;
                for (let x = 0; x < w; ++x) {
                    for (let y = 0; y < h; ++y) {
                        let dot = f.letters[letter][y * w + x];
                        if (dot) ctx.fillRect(x, y, 1, 1);
                    }
                }
            };

            this.bitmap = qr_create_canvas(w * n + 1, h, ctx => {
                let offset_x = 0;
                for (let l in f.letters) {
                    for (let x of [0, 2, 1]) {
                        for (let y of [0, 2, 1]) {
                            ctx.fillStyle = x == 1 && y == 1 ? '#fff' : '#000';
                            ctx.save()
                                ctx.translate(x, y);
                                render_letter(ctx, l);
                            ctx.restore();

                        }
                    }

                    this.char_offset[l] = offset_x;
                    offset_x += w;
                    ctx.translate(w, 0);
                }
            });
        }
    }

    function qr_render_string(f: qr_font_bitmap, ctx: CanvasRenderingContext2D, text: string, pos = v2()): void {
        let x = 0, y = 0;
        // ctx.drawImage(f.bitmap, pos.x, pos.y);
        for (let l of text.toUpperCase()) {
            if (l === '\n') {
                y += f.char_height;
                x = 0;
                continue;
            }
            ctx.drawImage(f.bitmap,
                f.char_offset[l], 0, f.char_width, f.char_height,
                x + pos.x, y + pos.y, f.char_width, f.char_height);
            x += f.char_width - 1;
        }
    }

    const qr_pixel_font = new qr_font_bitmap();


    function qr_create_canvas(width: number, height: number, cb?: (ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement) => void): HTMLCanvasElement {
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

    function qr_is_img_loaded(img: HTMLImageElement | HTMLCanvasElement): boolean {
        if (img instanceof HTMLImageElement) {
            return img && img.complete && img.naturalHeight !== 0;
        }
        return true;
    }

    class qr_sprite_data {
        constructor(
            public readonly image: HTMLImageElement | HTMLCanvasElement,
            public readonly negative: HTMLImageElement | HTMLCanvasElement,
            public readonly position: qm_vec,
            public readonly size: qm_vec
        ) { }

        public is_valid() {
            return qr_is_img_loaded(this.image);
        }
    }

    class qr_spritesheet {
        constructor(
            public readonly image: HTMLImageElement,
            public readonly negative_image: HTMLImageElement | HTMLCanvasElement,
            public readonly cell_size: qm_vec,
            public readonly grid_size: qm_vec
        ) { }

        public get_sprite(id: number): qr_sprite_data {
            let x = id % this.grid_size.x, y = Math.floor(id / this.grid_size.x);
            return new qr_sprite_data(this.image, this.negative_image, qm_mul(v2(x, y), this.cell_size), this.cell_size);
        }
    }

    class qr_sprite_sequence {
        public loop: boolean = false;
        protected elapsed = 0;
        protected total_time = 0;

        constructor(
            public readonly spritesheet: qr_spritesheet,
            public readonly frames: number[],
            public durations?: number[]
        ) { 
            if (this.durations) {
                qu_assert(this.frames.length == this.durations.length);
                this.total_time = this.durations.reduce((p, c) => p + c);
            } else {
                this.set_duration(1); 
            }
        }

        public set_duration(time: number): void {
            qu_assert(time > 0);
            this.durations = this.frames.map(_ => time / this.frames.length);
            this.total_time = this.durations.reduce((p, c) => p + c);
        }

        public tick(delta: number): void {
            this.elapsed += delta;

            if (this.loop) {
                if (this.elapsed > this.total_time) {
                    this.elapsed -= this.total_time;
                }
            }
        }

        public get_current_frame(): qr_sprite_data {
            for (let i = 0, time = 0; i < this.durations.length; ++i) {
                time += this.durations[i];
                if (this.elapsed < time) {
                    return this.spritesheet.get_sprite(this.frames[i]);
                }
            }

            return this.spritesheet.get_sprite(this.frames[this.frames.length - 1]);
        }
    }
    function qr_render_w(ctx: CanvasRenderingContext2D, world: qf_world)
    {
        qr_render_a(ctx, world.actors);
    }

    function qr_render_a(ctx: CanvasRenderingContext2D, actors: qf_actor[])
    {
        let primitives: qf_scene_component[] = [];

        for (let actor of actors)
        {
            for (let cmp of actor.components)
            {
                if (cmp instanceof qf_scene_component) primitives.push(cmp);
            }
        }

        qr_render_p(ctx, primitives);
    }

    function qr_render_p(ctx: CanvasRenderingContext2D, prmitives: qf_scene_component[])
    {
        for (let prim of prmitives) 
        {
            if (prim.visible) {
                prim.render_c2d(ctx);
            }
        }
    }

// system
    namespace qs_input
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
            type key_type = 'ArrowLeft' |
                       'ArrowRight' |
                       'ArrowUp' |
                       'ArrowDown' |
                       'z' | 'x' | ' ' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8';

            type q_keyboard_event = KeyboardEvent | q_fake_keyboard_event;

            const state: { [key:string]: key_state } =  { };
            const key_down_bindings: { [key:string]: qf_multicast_delegate<Function> } = { };

            export function bind_keydown(key: key_type, fn: Function, owner: qf_bindable): void {
                let b = key_down_bindings[key];
                if (!b) { b = key_down_bindings[key] = new qf_multicast_delegate(); }
                b.bind(fn, owner);
            }

            export function is_down(key: key_type) { 
                return state[key] ? state[key].is_down : false; 
            }

            export function get_state(key: key_type | string): key_state { 
                let s = state[key];
                if (!s) state[key] = s = new key_state();
                return s;
            }

            export function just_pressed(key: key_type, dt_ms = 20) {
                let ks = state[key];
                if (ks && ks.is_down) {
                    return Date.now() - ks.timestamp < dt_ms;
                }
                return false;
            }

            export function on_keydown(e: q_keyboard_event) {
                // #DEBUG-BEGIN
                if (e instanceof KeyboardEvent) {
                    g_poll_gamepad = false;
                }
                // #DEBUG-END
                let s = get_state(e.key);
                if (s.is_down) return;
                s.is_down = true;
                s.timestamp = Date.now();
                if (key_down_bindings[e.key]) {
                    key_down_bindings[e.key].broadcast();
                }
                e.preventDefault();
            }

            export function on_keyup(e: q_keyboard_event) {
                get_state(e.key).is_down = false;
            }

            class q_fake_keyboard_event {
                constructor(public key: key_type) { }
                preventDefault() { }
            }
            // #DEBUG-BEGIN
            let g_poll_gamepad = false;
            export function poll_gamepad() {
                let gp = navigator.getGamepads()[0];

                if (!g_poll_gamepad && gp) {
                    g_poll_gamepad = gp.buttons[0].pressed;
                }

                if (g_poll_gamepad && gp) {
                    if (gp.buttons[0].pressed) {
                        on_keydown(new q_fake_keyboard_event(' '));
                    } else if (get_state(' ').is_down) {
                        on_keyup(new q_fake_keyboard_event(' '));
                    }
                    if (gp.buttons[2].pressed) {
                        on_keydown(new q_fake_keyboard_event('z'));
                    } else if (gp.buttons[7].pressed) {
                        on_keydown(new q_fake_keyboard_event('z'));
                    } else if (get_state('z').is_down) {
                        on_keyup(new q_fake_keyboard_event('z'));
                    }

                    if (gp.axes[0] > 0.5) {
                        on_keydown(new q_fake_keyboard_event('ArrowRight'));
                    } else if (get_state('ArrowRight').is_down) {
                        on_keyup(new q_fake_keyboard_event('ArrowRight'));
                    }
                    if (gp.axes[0] < -0.5) {
                        on_keydown(new q_fake_keyboard_event('ArrowLeft'));
                    } else if (get_state('ArrowLeft').is_down) {
                        on_keyup(new q_fake_keyboard_event('ArrowLeft'));
                    }
                }
            }
            // #DEBUG-END
        }

        export namespace mouse
        {
            export const pos = v2();

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

// geometry
    const qf_c_jump_calc_offsets = [
        qm_top_left, qm_up, qm_top_right,
        qm_left, qm_right];

    // order of these offsets matter for algorithm execution
    const qf_c_breadth_search_offsets = [
        qm_left, qm_right, qm_up,
        qm_top_left, qm_top_right, qm_bottom_left,
        qm_bottom_right, qm_down];

    type qf_path_filter = (geom: qf_tile_geometry, s: qm_vec, e: qm_vec) => boolean;

    class qf_tile_geometry
    {
        //public blocking_tiles: boolean[][] = [];
        public blocking_dist: number[][] = [];
        public floor_dist: number[][] = [];
        public jump_dist: number[][] = [];

        // #DEBUG-BEGIN
        public d_considered: qm_vec[] = [];
        public d_hits: [qm_vec, qm_vec][] = [];
        // #DEBUG-END

        constructor(
            public readonly width,
            public readonly height,
            public readonly tile_size = 10
        ) {
            qu_assert(width > 0 && height > 0);

            for (let i = 0; i < height; ++i) {
                // this.blocking_tiles[i] = new Array(width).fill(false);
                this.blocking_dist[i] = new Array(width).fill(99);
                this.floor_dist[i] = new Array(width).fill(99);
                this.jump_dist[i] = new Array(width).fill(99);
            }
        }

        public set_blocking(x: number, y: number, blocking: boolean): void
        {
            // currently only support add blocking;
            qu_assert(blocking === true);
            let node = v2(x, y);
            if (this.is_valid(node)) 
            {
                // this.blocking_tiles[y][x] = blocking;
                this.update_blocking_dist(node, 0);
                this.update_floor_dist(node);
                this.update_jump_dist(node, 5);
            }
            else {
                qu_assert(false, `${x} != [0, ${this.width}) || ${y} != [0, ${this.height})`);
            }
        }
        
        public get_blocking_dist(n: qm_vec): number {
            return this.is_valid(n) ? this.blocking_dist[n.y][n.x] : Number.MAX_VALUE;
        }

        public get_floor_dist(n: qm_vec): number {
            return this.is_valid(n) ? this.floor_dist[n.y][n.x] : Number.MAX_VALUE;
        }

        public get_jump_dist(n: qm_vec): number {
            return this.is_valid(n) ? this.jump_dist[n.y][n.x] : Number.MAX_VALUE;
        }

        public is_valid(loc: qm_vec): boolean {
            return loc.x >= 0 && loc.x < this.width && loc.y >= 0 && loc.y < this.height;
        }

        protected update_blocking_dist(node: qm_vec, dist: number): void {
            let open: [qm_vec, number][] = [[node, dist]];

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

        protected update_floor_dist(loc: qm_vec): void {
            for (let pos = v2c(loc), dist = 0; pos.y >= 0; --pos.y, ++dist) {
                if (this.floor_dist[pos.y][pos.x] < dist) {
                    break;
                }
               this.floor_dist[pos.y][pos.x] = dist;
            }
        }

        protected update_jump_dist(root: qm_vec, range: number): void {
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

        public is_blocking(x: number, y: number): boolean {
            if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
                return this.blocking_dist[y][x] === 0;
            }
            return false;
        }

        public foreach_tile_along_path(start_loc: qm_vec, end_loc: qm_vec, fn: (x: number, y: number) => boolean): void
        {
            let s = this.project(start_loc);
            let e = this.project(end_loc);
            let d = qm_sub(e, s);

            const dir = qm_sub(end_loc, start_loc);

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
                    if (x == x_end) break;
                    x += d.x > 0 ? 1 : -1;
                }

                if (y != e.y) y += Math.sign(d.y);
                else break;
            }
        }

        public line_trace2(start_loc: qm_vec, end_loc: qm_vec): [qm_vec, qm_vec]
        {
            let hit_result: [qm_vec, qm_vec];

            this.foreach_tile_along_path(start_loc, end_loc, (x, y) => {
                // #DEBUG-BEGIN
                if (this.d_considered)
                    this.d_considered.push(qm_scale(v2(x, y), this.tile_size));
                // #DEBUG-END

                if (this.is_blocking(x, y)) {
                    hit_result = this.line_trace_tile(start_loc, end_loc, x, y);
                    if (hit_result) {
                        return true;
                    }
                }
            });

            return hit_result;
        }

        public line_trace_tile(start: qm_vec, end: qm_vec, x: number, y: number): qm_line_trace_result
        {
            const ts = this.tile_size;
            return qm_line_trace_aabb(start, end, [qm_scale(v2(x + 0.5, y + 0.5), ts), v2(ts / 2, ts / 2)]);
        }

        public sweep_aabb(start: qm_vec, end: qm_vec, size: qm_vec): qm_line_trace_result
        {
            let size_on_grid = v2(
                Math.max(0, Math.ceil(size.x / this.tile_size) + 1), 
                Math.max(0, Math.ceil(size.y / this.tile_size) + 1));

            let ext = v2(Math.floor(size_on_grid.x / 2), Math.floor(size_on_grid.y / 2));
            const ts = this.tile_size;
            const half_ts = qm_scale(v2(ts, ts), 0.5);
            const half_size = qm_scale(size, 0.5);
            const dir = qm_unit(qm_sub(end, start));

            let hit_result: qm_line_trace_result;

            this.foreach_tile_along_path(start, end, (x, y) => {

                let hits: qm_line_trace_result[] = [];

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
                            let tile_aabb: qm_aabb = [tile_center, qm_add(half_size, half_ts)];

                            if (qm_overlap_point(start, tile_aabb)) {
                                let d = qm_sub(start, end);
                                let hit = qm_line_trace_aabb(qm_add(start, qm_scale(d, 999)), start, tile_aabb);
                                qu_assert(!!hit);
                                hits.push(hit);
                                continue;
                            }

                            let hit = qm_line_trace_aabb(start, end, tile_aabb);
                            if (hit) {
                                hits.push(hit);
                            }
                        }
                    }
                }

                let best_dist = Number.MAX_VALUE;
                for (let hit of hits)
                {
                    let [p, _] = hit;
                    let dist = qm_mag_sqr(qm_sub(p, start));
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

        public find_path(start: qm_vec, end: qm_vec, is_conn_allowed: qf_path_filter): qm_vec[] {
            let s = this.project(start);
            let e = this.project(end);
            let id = (v: qm_vec) => v.y * this.width + v.x;
            let open: qm_vec[] = [s];
            let visited: number[] = [id(s)];
            let prev: number[] = [-1];

            while (open.length) {
                let start = open.shift();
                if (qm_eq(start, e)) break;

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

            let id_to_loc = (id: number) => {
                return qm_scale(v2(id % this.width + 0.5, Math.floor(id / this.width) + 0.5), this.tile_size);
            }

            let path: qm_vec[] = [id_to_loc(id(e))];
            for (let idx = visited.indexOf(id(e)); idx != -1;) {
                let tile_id = prev[idx];
                if (tile_id == -1) {
                    break;
                }

                path.unshift(id_to_loc(tile_id));
                idx = visited.indexOf(tile_id);
            }

            // #DEBUG-BEGIN
            if (g_draw_paths) {
                qi_g_paths.push(path);
            }
            // #DEBUG-END

            return path.length > 1 ? path : undefined;
        }

        public project(point: qm_vec): qm_vec
        {
            return v2(Math.floor(point.x / this.tile_size), Math.floor(point.y / this.tile_size));
        }
    }

// components
    abstract class qc_character_movement extends qf_component_base {
        // setup
        // any axis
        protected max_velocity = 300;
        // x axis
        public max_velocity_on_ground = 100;
        public gravity = 1000;
        public bounce_off_wall = false;
        public do_proces_input = true;

        // runtime data
        public vel = v2();
        public acc = v2();
        public last_hit: qm_line_trace_result;
        public on_ground = false;
        public moving_left = false;
        public wants_tick = true;

        public trace_wall(dist = 2): qm_line_trace_result {
            let r = this.owner.root;
            let g = this.owner.world.geometry as qf_tile_geometry;

            let trace = g.sweep_aabb(r.pos, qm_add(r.pos, qm_scale(qm_left, dist)), r.bounds);
            return trace ? trace : g.sweep_aabb(r.pos, qm_add(r.pos, qm_scale(qm_right, dist)), r.bounds);
        }

        public trace_ground(): qm_line_trace_result {
            let r = this.owner.root;
            let g = this.owner.world.geometry as qf_tile_geometry;
            return g.sweep_aabb(r.pos, qm_add(r.pos, v2(0, 5)), r.bounds);
        }

        public abstract process_input(): void;

        public tick(delta: number) {
            if (this.do_proces_input) {
                this.process_input();
            }

            let r = this.owner.root;
            let g = this.owner.world.geometry as qf_tile_geometry;

            this.vel.y += this.gravity * delta;
            this.vel = qm_add(this.vel, qm_scale(this.acc, delta));
            this.vel = qm_clamp_mag(this.vel, 0, this.max_velocity);

            let end = qm_add(r.pos, qm_scale(this.vel, delta));
            let trace = g.sweep_aabb(r.pos, end, r.bounds);

            if (trace) {
                let [p, n] = trace;
                this.last_hit = trace;

                if (n.y != 0) {
                    end.y = p.y
                    if (end.x != p.x) {
                        let x_trace = g.sweep_aabb(p, end, r.bounds);
                        if (x_trace) {
                            end.x = x_trace[0].x;
                        }
                    }
                    this.vel.y *= this.bounce_off_wall ? -qm_rnd(0.2, 0.5) : 0;
                }
                if (n.x != 0) {
                    end.x = p.x
                    if (end.y != p.y) {
                        let y_trace = g.sweep_aabb(p, end, r.bounds);
                        if (y_trace) {
                            end.y = y_trace[0].y;
                        }
                    }

                    this.vel.x *= this.bounce_off_wall ? -qm_rnd(0.2, 0.5) : 0;
                }
            }

            r.pos.x = end.x;
            r.pos.y = end.y;

            this.on_ground = !!this.trace_ground();
        }
    }

    class qc_sprite_component extends qf_scene_component {
        public sprite = new qr_sprite_data(null, null, qm_zero, v2(10, 10));
        public flip_x = false;
        public offset = v2();
        public negative = false;

        public blink(): void {
            // this.visible = false;
            this.negative = true;
            this.get_timer().delay(0.05, _ => this.visible = true, this);
            this.get_timer().delay(0.10, _ => this.negative = false, this);
        }

        public render_c2d_impl(ctx: CanvasRenderingContext2D): void {
            const sp = this.sprite.position,
                  ss = this.sprite.size;

            if (this.flip_x) {
                ctx.scale(-1, 1);
            }

            if (this.sprite.is_valid()) {
                ctx.drawImage(this.negative ? this.sprite.negative : this.sprite.image, 
                    sp.x, sp.y, ss.x, ss.y,
                    -ss.x / 2 + this.offset.x, -ss.y / 2 + this.offset.y, ss.x, ss.y);
            } else {
                ctx.fillStyle = 'red';
                ctx.fillRect(-ss.x/2,  -ss.y/2, ss.x, ss.y);
            }
        }
    }

    class qc_anim_sprite_component extends qc_sprite_component {
        public sequences: {[name:string]: qr_sprite_sequence} = {}
        public current_sequence = '';
        public wants_tick = true;

        public play(name: string): void {
            if (this.current_sequence != name) {
                qu_assert(!!this.sequences[name]);
                this.current_sequence = name;
                this.sprite = this.sequences[name].get_current_frame();
            }
        }

        public tick(delta: number): void {
            if (this.current_sequence) {
                let seq = this.sequences[this.current_sequence];
                seq.tick(delta);
                this.sprite = seq.get_current_frame();
            }
        }
    }

    class qc_label_component extends qf_scene_component {
        protected text = '';
        public font = qr_pixel_font;
        public set_text(s: string): void {
            let l = s.split('\n');
            this.bounds.x = l.map(l => l.length).sort().pop() * (this.font.char_width - 1) + 1;
            this.bounds.y = l.length * (this.font.char_height + 1);
            this.text = s;
        }
        public render_c2d_impl(ctx: CanvasRenderingContext2D): void {
            qr_render_string(this.font, ctx, this.text, qm_scale(this.bounds, -0.5));
        }
    }

    function apply_radial_damage(src: qf_actor, force = 300, radious = 50, damage = 1, cc = qf_cc.enemy) {
        let w = src.world;
        let p = src.get_pos();

        for (let a of w.actors) {
            if (a.root && (a.root.collision_mask & cc)) {
                let dir = qm_sub(a.root.get_pos(), p);
                if (qm_mag(dir) <= radious) {
                    let m = a.getcmp(qi_ai_movement)[0];
                    if (m) {
                        m.vel = qm_clamp_mag(dir, force, 0);
                    }
                    if (damage > 0) {
                        a.on_take_damage.broadcast(new qf_damage_event(damage, src, qm_unit(qm_scale(dir, -1))));
                    }
                }
            }
        }
    }

    class qc_explosion_component extends qf_scene_component {
        public wants_tick = true;
        public duration = 0.11;
        public elpased = 0;
        public radious = 10;

        public begin_play() {
            apply_radial_damage(this.owner, 200, this.radious, 1);
        }

        public tick(delta: number) {
            if (this.elpased > this.duration) {
                this.owner.destroy();
                g_scene_offset = v2();
            }
            g_scene_offset.x = qm_rnd(-1, 1);
            g_scene_offset.y = qm_rnd(-1, 1);
            this.elpased += delta;
        }

        public render_c2d_impl(ctx: CanvasRenderingContext2D): void {
            let alpha = this.elpased / this.duration;
            let gco = ctx.globalCompositeOperation;

            if (alpha > 0.5) {
                ctx.globalCompositeOperation = 'difference';
            }
            ctx.beginPath();
            ctx.arc(0, 0, this.radious, 0, 2*Math.PI);
            ctx.fillStyle =  alpha < 0.5 ? '#000': '#fff';
            ctx.fill();
            ctx.globalCompositeOperation = gco;
        }
    }

    class qc_wave_component extends qf_scene_component {
        public wants_tick = true;
        public start_radious = 10;
        public end_radious = 400;
        public expand_durtation = 0.25;
        public slomo_duration = 0.15;
        protected elapsed = 0;

        public begin_play(): void {
            apply_radial_damage(this.owner, 600, 99999, 0);
        }

        public tick(delta: number): void {
            this.elapsed += delta;

            if (this.elapsed < this.slomo_duration) {
                const hs = this.slomo_duration / 1.05;
                g_time_dilation = qm_clamp(1 - this.elapsed / hs, 0.01, 1);
            }
            if (this.elapsed > this.slomo_duration) {
                g_time_dilation = 1;
            }
            if (this.elapsed > this.expand_durtation) {
                this.owner.destroy();
            }
        }

        public render_c2d_impl(ctx: CanvasRenderingContext2D): void {
            let a = qm_clamp(this.elapsed / this.expand_durtation, 0, 1);
            ctx.beginPath();
            ctx.arc(0, 0, this.start_radious + (this.end_radious - this.start_radious) * a, 0, 2*Math.PI);

            let gco = ctx.globalCompositeOperation;
            ctx.globalCompositeOperation = 'difference';
            ctx.strokeStyle = '#fff';
            ctx.lineWidth = 3;
            ctx.stroke();
            ctx.globalCompositeOperation = gco;
        }
    }

    class qc_pickable_component extends qf_component_base {

        public on_overlap_begins = new qf_multicast_delegate();
        public on_overlap_ends = new qf_multicast_delegate();

        public lifespan = 0;
        public elapsed = 0;
        public blink = false;
        public wants_tick = true;
        public bounds: qm_aabb;

        protected last_frame_overlap = false;

        public begin_play(): void {
            // this.get_timer().every(0.05, this.update, this);
            if (this.lifespan > 0) {
                this.get_timer().delay(this.lifespan, this.owner.destroy, this.owner);
                if (this.blink) {
                    this.get_timer().every(0.1, this.blink_impl, this);
                }
            }
        }

        public tick(delta: number) {
            let r = this.owner.root;
            let w = this.get_world();
            this.elapsed += delta;
            
            let o = w.overlap(this.bounds ? this.bounds : r.get_aabb(), qf_cc.player);
            if (o[0] && !this.last_frame_overlap) {
                this.on_overlap_begins.broadcast();
                this.last_frame_overlap = true;
            }
            else if (!o[0] && this.last_frame_overlap) {
                this.on_overlap_ends.broadcast();
                this.last_frame_overlap = false;
            }

        }

        public blink_impl() {
            let r = this.owner.root;
            if (this.lifespan > 0 && (this.lifespan - this.elapsed) < 1) {
                r.visible = !r.visible;
            }
        }
    }

    // class qc_tween_manager extends qf_component_base {
    //     public tweens: qf_tween[] = [];
    //     public wants_tick = true;

    //     public tick(delta: number): void {
    //         for (let i = this.tweens.length - 1; i >= 0; --i) {
    //             this.tweens[i].tick(delta);
    //             if (this.tweens[i].is_done()) {
    //                 this.tweens.splice(i, 1);
    //             }
    //         }
    //     }
    //     public make<T, K extends keyof T>(obj: T, prop: K, start_value: number, end_value: number, duration: number, easing = qf_easing_sin_inout): qf_tween {
    //         let t = new qf_tween(obj, prop, start_value, end_value, duration, easing);
    //         this.tweens.push(t);
    //         return t;
    //     }
    // }

    class qc_player_movement extends qc_character_movement
    {
        public process_input() {
            const speed = 900;

            this.acc.x = 0;
            this.acc.y = 0;

            let k = qs_input.keyboard;
            const move_left = k.is_down('ArrowLeft');
            const move_right = k.is_down('ArrowRight');
            const jump =  k.just_pressed('ArrowUp') || k.just_pressed(' ');

            if (move_left || move_right) {
                // this.acc.x = speed * (this.on_ground ? 1 : 0.5);
                this.acc.x = move_left ? -speed : speed;
                this.moving_left = move_left;
                this.vel.x = qm_clamp(this.vel.x, -this.max_velocity_on_ground, this.max_velocity_on_ground);
            }
            else if (this.on_ground)
            {
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

        public tick(delta: number)
        {
            delta = delta * 1/g_time_dilation;
            this.process_input();
            super.tick(delta);
        }
    }

    class qc_projectile_movement extends qf_component_base
    {
        public acc = v2(0, 1000);
        public vel = v2();
        public lifespan = 0;
        public damage = 1;
        public rotate_root = true;
        public bounce_off_walls = false;
        public bounce_factor = 0.5; // 1 = no velocity loss
        public instigator: qf_actor = null;
        public collision_channel: qf_cc = qf_cc.all;
        public on_hit = new qf_multicast_delegate<(hit_result: qf_hit_result, bullet: this) => void>();
        public wants_tick = true;

        public begin_play() {
            if (this.lifespan > 0) {
                this.get_timer().delay(this.lifespan, _ => this.owner.destroy(), this.owner);
            }
        }

        public tick(delta: number): void
        {
            let r = this.owner.root;
            let w = this.owner.world;

            this.vel = qm_add(this.vel, qm_scale(this.acc, delta));
            let ds = qm_scale(this.vel, delta);
            let end = qm_add(r.pos, ds);

            let hit_result = w.sweep_aabb(r.pos, end, r.bounds, this.collision_channel);
            if (hit_result)
            {
                if (this.bounce_off_walls) {
                    if (hit_result.normal.x != 0) this.vel.x *= -this.bounce_factor;
                    if (hit_result.normal.y != 0) this.vel.y *= -this.bounce_factor;
                }
                else {
                    this.vel = v2();
                    this.acc = v2();
                    this.wants_tick = false;
                }

                r.pos = v2c(hit_result.pos);
                this.on_hit.broadcast(hit_result, this);
            }
            else
            {
                r.pos = end;
                if (this.rotate_root) {
                    r.rot = Math.atan2(this.vel.y, this.vel.x);
                }
            }
        }
    }

    class qc_player_controller extends qf_component_base
    {
        private movement: qc_player_movement;
        private sprite: qc_anim_sprite_component;
        private weapon_sprite: qc_sprite_component;
        private stats: qc_label_component;

        // bullet setup
        public bullet_speed = 300;
        public fire_spread = 5;
        public bullets_per_shoot = 1;
        public explosion_chance = 0.1;
        public bullet_damage = 1;

        // other setup
        public gems_per_kill = 3;

        public life = 2;
        public gem_count = 0;
        public should_take_damage = true;
        public wants_tick = true;

        protected fire_rate = 4;
        protected fire_delegate: Function;
        protected timer = new qf_timer();

        public begin_play()
        {
            [this.movement] = this.owner.getcmp(qc_player_movement);
            [this.sprite]   = this.owner.getcmp(qc_anim_sprite_component);
            [this.stats] = this.owner.getcmp(qc_label_component);
            [this.weapon_sprite] = this.owner.getcmp_byname('weapon_sprite') as qc_sprite_component[];
            this.owner.on_take_damage.bind(this.take_damage, this);
            this.set_fire_rate(this.fire_rate);

            // #DEBUG-BEGIN
            qs_input.keyboard.bind_keydown('1', _ => g_world.actors.forEach(a => a.getcmp(qi_enemy_controller)[0] ? a.on_take_damage.broadcast(new qf_damage_event(9999, this.owner, qm_up)) : 1), this);
            qs_input.keyboard.bind_keydown('2', _ => reset(), this);
            qs_input.keyboard.bind_keydown('3', _ => g_game_mode.spawn_wave(), this);
            qs_input.keyboard.bind_keydown('5', _ => qm_rnd_select(spawn_bat, spawn_goblin)(g_world, v2(30, 30)), this);
            qs_input.keyboard.bind_keydown('6', _ => this.life++, this);
            qs_input.keyboard.bind_keydown('7', _ => this.gem_count++, this);
            // #DEBUG-END
        }

        public tick(delta: number): void
        {
            this.timer.tick(delta * 1/g_time_dilation);
            let a = this.owner;

            this.sprite.flip_x = !this.movement.moving_left;
            this.weapon_sprite.pos.x = this.movement.moving_left ? -10 : 10;
            this.weapon_sprite.flip_x = !this.movement.moving_left;
            this.weapon_sprite.rot *= 0.9;

            let vel = this.movement.vel;

            if (this.movement.on_ground) {
                if (qm_eq_eps(vel.x, 0, 3)) {
                    this.sprite.play('idle');
                } else {
                    this.sprite.play('walk');
                }
            } else {
                this.sprite.play('jump');
            }

            if (qs_input.keyboard.is_down('z')) {
                this.fire_delegate();
            }

            this.stats.set_text(`lives: ${this.life}\ngems:  ${this.gem_count}\nwave:  ${g_stage}`);
            this.stats.pos = qm_add(v2(20, 20), qm_scale(this.stats.bounds, 0.5));
        }

        protected take_damage(e: qf_damage_event): void {
            if (!this.should_take_damage) {
                return;
            }

            this.movement.vel.x += e.dir.x * 100;
            this.movement.vel.y -= 100;
            this.sprite.blink();

            this.life -= 1;
            spawn_freeze_wave(this.get_world(), qm_add(this.owner.get_pos(), v2(0, 3)));

            if (this.life > 0) {
                // just for shake
                spawn_explosion(this.get_world(), {x:-100, y:-100, duration: 0.1});
                // invicibly frame
                this.should_take_damage = false;
                this.get_timer().delay(0.5, _ => this.should_take_damage = true, this);
            }
            else {
                this.get_timer().delay(0.05, _ => {
                    this.get_world().actors.forEach(a => a.getcmp(qc_wave_component)[0] && a.destroy());
                    g_time_dilation = 0.01;
                }, this.owner);
                this.sprite.play('dead');
                g_game_mode.print(`    you died\nwaves survived: ${g_stage}\n\npress z to restart`);
                qs_input.keyboard.bind_keydown('z', _ => reset(), this.owner);
                this.owner.components = this.owner.components.filter(c => {
                    return !((c instanceof qc_player_movement) || (c instanceof qc_player_controller)); });
            }
        }

        public set_fire_rate(rate: number): void {
            this.fire_rate = rate;
            this.fire_delegate = this.timer.throttle(1/qm_clamp(rate, 0.5, 1000), this.fire, this);
        }

        protected fire(): void {

            let a = this.owner;
            let ml = this.movement.moving_left;

            let dir = ml ? qm_left: qm_right;
            let angle = qm_clamp((this.bullets_per_shoot - 1) * 0.087, 0, 0.785) * dir.x;
            let step = 0;
            if (this.bullets_per_shoot > 1) {
                step = (2 * angle) / (this.bullets_per_shoot - 1)
            }
            dir = qm_rotate(dir, -angle);

            for (let i = 0; i < this.bullets_per_shoot; ++i)
            {
                spawn_bullet(a.world, this.weapon_sprite.get_pos(), 
                    qm_scale(dir, this.bullet_speed * 1 / g_time_dilation), this.owner,
                    {
                        lifespan: 0.3, gravity: 0, cc: qf_cc.enemy | qf_cc.geom,
                        damage: this.bullet_damage,
                        explosion_chance: this.explosion_chance
                    });
                dir = qm_rotate(dir, step);
            }
            
            if (this.movement.on_ground)
                this.movement.vel.x += dir.x * -0.1;

            this.weapon_sprite.rot = this.movement.moving_left ? 3.14/2 :-3.14/2;
        }

        public apply_upgrade(item: qg_item_type): void {
            switch (item) {
                case qg_item_type.bullet_damage_upgrade: this.bullet_damage += 1; return;
                case qg_item_type.bullet_explosion_upgrade: this.explosion_chance = qm_clamp(this.explosion_chance + 0.1, 0, 1); return;
                case qg_item_type.bullet_num_upgrade: this.bullets_per_shoot += 1; return;
                case qg_item_type.bullet_range_upgrade: this.bullet_speed += 50; return;
                case qg_item_type.fire_rate_upgrade: this.set_fire_rate(this.fire_rate + 2); return;
                case qg_item_type.life_upgrade: this.life += 1; return;
                case qg_item_type.coin_drop_upgrade: this.gems_per_kill += 2; return;
                case qg_item_type.finish_shopping: g_game_mode.finish_shopping(); return;
            }
            qu_assert(false);
        }
    }

    
    type wave_spawn_def = [(w: qf_world, p: { x: number, y: number }) => qf_actor, number];
    class qc_game_mode extends qf_component_base {

        protected wave_def: wave_spawn_def[][] = [
            [[spawn_slime, 2], [spawn_bat, 1]],
            [[spawn_slime, 4], [spawn_bat, 2]],
            [],
            [[spawn_slime, 8], [spawn_bat, 4]],
            [[spawn_slime, 0], [spawn_bat, 8]],
            [],
            [[spawn_boss1, 1]],
            [],
            [[spawn_goblin, 2], [spawn_bat, 4]],
            [[spawn_goblin, 4], [spawn_bat, 4]],
            [],
            [[spawn_goblin, 2], [spawn_boss1, 1]],
            [[spawn_goblin, 8], [spawn_bat, 0]],
            [],
            [[spawn_boss1, 2]] 
        ];
        protected center_label: qc_label_component;
        protected print_timer_handle: qf_timer_event;

        public spawned_actors: qf_actor[] = [];
        
        public begin_play(): void {
            [this.center_label] = this.owner.getcmp(qc_label_component);
            qi_g_on_enemy_killed.bind(this.on_enemy_killed, this);
            this.get_timer().delay(2, this.spawn_wave, this);
            this.print(`game title\n\ngame by kakus for js13k 2018`, 5);
        }

        public print(msg: string, lifespan: number = 0) {
            this.center_label.visible = true;
            this.center_label.set_text(msg);
            if (this.print_timer_handle) {
                this.print_timer_handle.invalidate();
            }
            if (lifespan) {
                this.print_timer_handle = this.get_timer().delay(lifespan, _ => this.center_label.visible = false, this);
            }
        }

        public on_enemy_killed(e: qf_actor): void {
            if (this.spawned_actors.every(a => !a.is_valid() || a === e)) {
                qu_log(`wave ${g_stage} finished`);
                g_stage += 1;
                this.get_timer().delay(3, this.spawn_wave, this);
            }
        }

        public finish_shopping(): void {
            if (!this.is_shopping_time()) return;
            this.spawned_actors.forEach(a => a.is_valid() && a.destroy());
            this.on_enemy_killed(undefined);
        }

        public spawn_wave(): void {
            let w = this.get_world();
            let wave = this.wave_def[g_stage % this.wave_def.length];
            let point_num = g_spawn_positions.length;
            let spawn_num = 0;
            this.spawned_actors = [];

            if (this.is_shopping_time()) {
                let upg = [];
                for (let i = 0; i < qg_item_type.max_none; ++i) {
                    upg[i] = i;
                }
                let idx = qg_item_type.finish_shopping;
                for (let pos of g_item_positions) {
                    this.spawned_actors.push(spawn_shop_item(w, {x: pos.x, y: pos.y, item: upg[idx]}));
                    upg.splice(idx, 1);
                    idx = qm_rnd(0, upg.length - 1) | 0;
                }
                let v = spawn_vendor(w, g_npc_positions[0]);
                this.spawned_actors.push(v);

                let shoppin_time = 20;
                this.get_timer().delay(shoppin_time, this.finish_shopping, this);
                this.get_timer().every(1, _ => {
                    this.print(`shopping ends in ${--shoppin_time}`, 1.1);
                }, v)
            }

            for (let [_, num] of wave) {
                for (let i = 0; i < num; ++i) {
                    let p = g_spawn_positions[spawn_num % point_num];
                    spawn_spawn_portal(w, p);
                    spawn_num += 1;
                }
            }

            this.get_timer().delay(1, _ => {
                spawn_num = 0;
                for (let [spawn_fn, num] of wave) {
                    for (let i = 0; i < num; ++i) {
                        this.spawned_actors.push(spawn_fn(w, g_spawn_positions[spawn_num % point_num]));
                        spawn_num += 1;
                    }
                }
            }, this);
        }

        protected is_shopping_time(): boolean {
            return this.wave_def[g_stage % this.wave_def.length].length === 0;
        }
    }

// ai
    var qi_g_paths: qm_vec[][] = [];
    const qi_g_on_enemy_killed = new qf_multicast_delegate<(a: qf_actor) => void>();

    function qi_flying_path_filter(geom: qf_tile_geometry, s: qm_vec, e: qm_vec): boolean {
        if (geom.is_blocking(e.x, e.y)) return false;
        // let d = qm_sub(e, s);
        // // if moving diagonal
        // if (qm_manhattan_dist(d) == 2) {
        //     // forbid if near blocking geom
        //     if (geom.is_blocking(e.x, s.y)) return false;
        // }
        return true;
    }

    function qi_default_walk_filter(geom: qf_tile_geometry, s: qm_vec, e: qm_vec): boolean {
        if (geom.is_blocking(s.x, s.y)) return false;

        let e_dist = geom.get_blocking_dist(e);

        // end is blocking tile
        if (e_dist == 0) return false;

        let dir = qm_sub(e, s);
        let e_floor = geom.get_floor_dist(e);

        // we can move horizontaly only on tiles that are floors
        if (dir.y == 0 && e_floor > 1) return false;

        let s_jump = geom.get_jump_dist(s);

        // if we want to go up
        if (dir.y < 0) {
            // we can go straight up only near walls
            if (dir.x == 0 && e_dist > 1) return false;
            // we can keep goin up while we are near floor or walls
            if (dir.x != 0 && s_jump > 2) return false;
        }
        return true;
    }

    class qi_ai_movement extends qc_character_movement
    {
        // setup
        public ground_acc = 500;
        public air_acc  = 500;
        public jump_vel = 300;
        public flying = false;
        public default_max_velocity = 300;

        // runtime
        public input = v2();

        public tick(delta: number): void {
            this.do_proces_input = g_time_dilation == 1;
            this.max_velocity = g_time_dilation != 1 ? 9999 : this.default_max_velocity;
            super.tick(delta);
        }

        public process_input() {

            if (this.flying) {
                this.acc = qm_scale(this.input, this.air_acc);
                let wall = this.trace_wall(3);
                if (wall) {
                    this.vel.x = wall[1].x * 80;
                }
                return;
            }

            if (this.input.x != 0) {
                this.acc.x = this.input.x * (this.on_ground ? this.ground_acc : this.air_acc);
                this.vel.x = qm_clamp(this.vel.x, -this.max_velocity_on_ground, this.max_velocity_on_ground);
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

    class qi_enemy_controller extends qf_component_base
    {
        public root: qf_scene_component;
        public mov: qi_ai_movement;
        public spr: qc_anim_sprite_component;


        public target: qf_actor;
        public hitpoints = 1;
        public base_damage = 1;
        public gems = 1;

        public atk_speed = 0.5;
        public atk_lock = false;

        protected last_hit: qf_damage_event;

        public begin_play()
        {
            super.begin_play();
            [this.mov] = this.owner.getcmp(qi_ai_movement);
            [this.spr] = this.owner.getcmp(qc_anim_sprite_component);

            this.target = this.owner.world.player;
            this.root = this.owner.root;
            this.owner.on_take_damage.bind(this.take_damage, this);
            this.get_timer().every(0.1, this.update, this);
        }

        protected update(): void {
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
        
        public take_damage(e: qf_damage_event) {
            this.last_hit = e;
            this.hitpoints -= e.damage;

            if (this.root instanceof qc_sprite_component) {
                this.root.blink();
            }
            
            if (this.hitpoints <= 0) {
                this.handle_death();
            }
        }

        public handle_death(): void {
            let w = this.get_world();

            for (let i = qm_rnd(this.gems, this.gems + g_player_controller.gems_per_kill); i >= 0; --i) {
                let coin = spawn_coin(w, this.owner.get_pos()).getcmp(qc_projectile_movement)[0];
                let lh = this.last_hit.dir;
                coin.vel.x = (lh.x != 0 ? lh.x : 1) * -qm_rnd(100, 200);
                coin.vel.y = qm_rnd(200, 400);
            }

            if (g_time_dilation < 1) {
                spawn_freeze_wave(w, this.owner.get_pos());
            }

            qi_g_on_enemy_killed.broadcast(this.owner);
            this.owner.destroy();
        }

        protected overlaps_target(): boolean {
            return qm_overlap_aabb(this.root.get_aabb(), this.target.root.get_aabb());
        }
    }

    class qi_slime_controller extends qi_enemy_controller
    {
        public begin_play()
        {
            super.begin_play();
            this.mov.bounce_off_wall = true;

            this.get_world().timer.delay(qm_rnd(), _ => {
                this.get_world().timer.every(qm_rnd(2.6, 3), this.do_jump.bind(this), this.owner);
            }, this.owner);
        }

        public do_jump(): void
        {
            let dir = qm_sign(qm_sub(this.target.get_pos(), this.root.pos));
            let w = this.get_world();
            let start = this.root.pos;

            let g = this.owner.world.geometry as qf_tile_geometry;
            let path = g.find_path(this.root.pos, this.target.root.pos, qi_default_walk_filter);
            if (path) {
                for (let i = 1; i < path.length; ++i) {
                    dir = qm_sign(qm_sub(path[i], path[i - 1]));
                    if (dir.x != 0) break;
                }
            }

            let jump = qm_scale(v2(300 * dir.x, -300), qm_rnd(0.5, 1));

            let hit = w.sweep_aabb(start, qm_add(start, v2(20 * dir.x, -20)), this.root.bounds, qf_cc.geom);
            this.mov.vel = hit ? qm_mul(jump, v2(0.2, 1.5)) : jump;
        }
    }

    class qi_humanoid_controller extends qi_enemy_controller {
        public think_event: qf_timer_event;
        public path_filter: qf_path_filter = qi_default_walk_filter;
        public dimishing_return = 1;

        public begin_play() {
            super.begin_play()
            this.think_event = this.get_timer().every(0.033, this.think, this);
        }

        public think(delta: number) {
            if (this.atk_lock) {
                return;
            }

            this.dimishing_return = qm_clamp(this.dimishing_return + 0.01, 0, 1);

            let g = this.owner.world.geometry as qf_tile_geometry;
            let path = g.find_path(this.root.pos, this.target.root.pos, this.path_filter);
            if (path) {
                this.mov.input = qm_scale(qm_sign(qm_sub(path[1], path[0])), qm_rnd(0.5, 1));
            }
            else {
                this.mov.input.x = 0;
                this.mov.input.y = 0;
            }
        }



        public take_damage(e: qf_damage_event): void {
            this.think_event.fire_in = 0.5 * this.dimishing_return;
            this.mov.input = v2();
            this.mov.vel.x -= 200 * e.dir.x * this.dimishing_return * (this.mov.flying ? 0.5 : 1);
            this.mov.vel.y -= 160 * this.dimishing_return * (this.mov.flying ? 0 : 1);

            this.dimishing_return = qm_clamp(this.dimishing_return - 0.1, 0, 1);
            super.take_damage(e);
        }
    }

    class qc_boss_controller extends qi_humanoid_controller {

        public begin_play() {
            super.begin_play();
            this.get_timer().every(5, _ => {
                for (let i = 0; i < 3; ++i)
                    this.get_timer().delay(0.1 * i, this.try_fire, this);
                }, this);
        }

        public think(delta: number) {
            super.think(delta);
            this.spr.play('walk');            
            this.dimishing_return = 0;
        }

        public try_fire(): void {
            let w = this.get_world();
            let this_pos = this.owner.get_pos();
            let trg_pos = this.target.get_pos();

            this.think_event.fire_in = 0.5;
            this.spr.play('fire');            
            this.mov.input = v2();
            this.mov.vel = v2();

            let dir = qm_clamp_mag(qm_sub(trg_pos, this_pos), 200);
            spawn_bullet(w, this_pos, dir, this.owner, { lifespan: 3, gravity: 0, cc: qf_cc.player});
        }
    }


    /**
     * GLOBALS
     */
    const g_spritesheet_image = (_ => { let i = new Image(); i.src = 's.png'; return i; })();
    const g_negative_spritesheet_image = qr_create_canvas(128, 128);
    const g_character_spritesheet = new qr_spritesheet(g_spritesheet_image, g_negative_spritesheet_image, v2(14, 18), v2(9, 9));
    const g_tile_spritesheet = new qr_spritesheet(g_spritesheet_image, g_negative_spritesheet_image, v2(14, 14), v2(9, 10));
    let g_stage = 0;
    let g_spawn_positions: qm_vec[] = [];
    let g_item_positions: qm_vec[] = [];
    let g_npc_positions: qm_vec[] = [];
    let g_scene_offset = v2();

    /**
     *  SPAWN METHODS
     */
    function spawn_slime(world: qf_world, {x, y}): qf_actor
    {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), {x, y, coll_mask: qf_cc.enemy, root: true});
        r.bounds = v2(9, 6);
        r.sequences['idle'] = new qr_sprite_sequence(g_character_spritesheet, [4, 5], [.12, .12]);
        r.sequences['idle'].loop = true; 
        r.play('idle');
        r.offset.y -= 3;
    
        qf_attach_cmp(a, new qi_ai_movement());
        let c = qf_attach_cmp(a, new qi_slime_controller());
        c.hitpoints = 2;
        return a;
    }

    function spawn_goblin(world: qf_world, {x, y}): qf_actor
    {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), {x, y, coll_mask: qf_cc.enemy, root: true});
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
        h.hitpoints = 8;
        return a;
    }

    function spawn_bat(world: qf_world, {x, y}): qf_actor
    {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), {x, y, coll_mask: qf_cc.enemy, height: 12, root: true});
        let w = r.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [6, 7]);
        w.loop = true;
        w.set_duration(0.25);
        r.play('walk');

        let mov = qf_attach_cmp(a, new qi_ai_movement());
        mov.gravity = 0;
        mov.flying = true;
        mov.bounce_off_wall = true;
        mov.default_max_velocity = 60;

        let h = qf_attach_cmp(a, new qi_humanoid_controller());
        h.hitpoints = 3;
        h.path_filter = qi_flying_path_filter;
        return a;
    }

    function spawn_boss1(world: qf_world, {x, y}): qf_actor
    {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), {x, y, coll_mask: qf_cc.enemy, height: 14, width: 13, root: true});
        let w = r.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [37, 38]);
        w.loop = true;
        w.set_duration(0.25);
        r.play('walk');
        r.offset.y = -1;
        r.sequences['fire'] = new qr_sprite_sequence(g_character_spritesheet, [36]);
        // r.scale = v2(2, 2);

        let mov = qf_attach_cmp(a, new qi_ai_movement());
        mov.gravity = 0;
        mov.flying = true;
        mov.bounce_off_wall = true;
        mov.default_max_velocity = 50;

        let h = qf_attach_cmp(a, new qc_boss_controller());
        h.hitpoints = 20;
        h.gems = 30;
        h.path_filter = qi_flying_path_filter;
        return a;
    }

    function spawn_spawn_portal(world: qf_world, { x, y }): qf_actor {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), { x, y, root: true });
        let w = r.sequences['1'] = new qr_sprite_sequence(g_character_spritesheet, [8, 13]);
        w.loop = true;
        w.set_duration(0.02);
        r.play('1');
        r.offset.y = -1;

        world.timer.delay(1, _ => a.destroy(), a);
        return a;
    }

    function spawn_coin(world: qf_world, {x, y}): qf_actor {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_sprite_component(), {x, y, width: 8, height: 7, root: true});
        r.sprite = g_character_spritesheet.get_sprite(33);

        let m = qf_attach_cmp(a, new qc_projectile_movement());
        m.bounce_off_walls = true;
        m.bounce_factor = 0.8;
        m.rotate_root = false;
        m.collision_channel = qf_cc.geom;

        let p = qf_attach_cmp(a, new qc_pickable_component());
        p.on_overlap_begins.bind(_ => { g_player_controller.gem_count += 1; a.destroy(); }, p);
        p.lifespan = 4;
        p.blink = true;

        return a;
    }

    function spawn_freeze_wave(world: qf_world, {x, y}): qf_actor {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_wave_component(), {x, y, root: true});
        return a;
    }

    function spawn_explosion(world: qf_world, {x, y, radious = 15, duration = 0.11}): qf_actor {
        let a = world.spawn_actor();
        let e = qf_attach_prim(a, new qc_explosion_component(), {x, y, root: true});
        e.radious = (qm_rnd_normal() + 1) * radious;
        e.duration = duration;
        return a;
    }

    function spawn_shop_item(world: qf_world, {x, y, item = qg_item_type.max_none}): qf_actor {
        let a = world.spawn_actor();
        let s = qf_attach_prim(a, new qc_sprite_component(), {x, y, width: 14, height: 16, root: true});
        s.sprite = g_character_spritesheet.get_sprite(34);

        let cost = get_item_cost(item);
        cost += Math.sign(cost) * (5 * ((g_stage/3)|0));

        let l = qf_attach_prim(a, new qc_label_component(), {y: -25});
        l.parent = s;
        l.set_text(get_item_desc(item) + '\ncost: ' + cost + '\njump to buy');
        l.visible = false;

        let p = qf_attach_cmp(a, new qc_pickable_component());
        p.on_overlap_begins.bind(_ => l.visible = true, p);
        p.on_overlap_ends.bind(_ => l.visible = false, p);

        let p2 = qf_attach_cmp(a, new qc_pickable_component());
        p2.bounds = [v2(x, y - 40), v2(10, 10)];
        p2.on_overlap_begins.bind(_ => {
            if (g_player_controller.gem_count >= cost) {
                spawn_freeze_wave(world, {x, y});
                g_player_controller.apply_upgrade(item);
                if (a.is_valid()) {
                    a.destroy();
                }
                g_player_controller.gem_count -= cost;
            }
        }, p2);

        // let t = qf_attach_cmp(a, new qc_tween_manager());
        // t.make(s.offset, 'y', 0, -10, 1)
        //  .then(s.offset, 'y', -10, 0, 1)
        //  .loop();

        return a;
    }

    function spawn_vendor(world: qf_world, {x, y}): qf_actor {
        let a = world.spawn_actor();
        let r = qf_attach_prim(a, new qc_anim_sprite_component(), {x, y, coll_mask: qf_cc.enemy, height: 12, root: true});
        r.offset.y = -2;
        let idle = r.sequences['idle'] = new qr_sprite_sequence(g_character_spritesheet, [14, 15]);
        idle.loop = true;
        idle.set_duration(0.5);
        r.play('idle');

        let walk = r.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [16, 17]);
        walk.loop = true;
        walk.set_duration(0.25);

        let mov = qf_attach_cmp(a, new qi_ai_movement());
        mov.max_velocity_on_ground *= 1.5;
        mov.air_acc *= 1.5;

        let h = qf_attach_cmp(a, new qi_humanoid_controller());
        h.atk_lock = true;
        h.hitpoints = 999;
        h.gems = 200;

        let l = qf_attach_prim(a, new qc_label_component(), {y:-20});
        l.parent = r;
        l.set_text(qm_rnd_select('shopping time', 'only best deals', 'sale sale!'));
        world.timer.delay(2, _ => l.visible = false, a);

        a.on_take_damage.bind(_ => { h.atk_lock = false; r.play('walk'); }, a);
        return a;
    }

    // #DEBUG-BEGIN
    export var g_draw_considered = false;
    export var g_draw_blocking_dist = false;
    export var g_draw_floor_dist = false;
    export var g_draw_jump_dist = false;
    export var g_draw_id = false;
    export var g_draw_bounds = false;
    export var g_draw_paths = false;

    class debug_draw_collisions extends qf_scene_component
    {
        public tiles: qf_tile_geometry;

        public begin_play()
        {
            this.tiles = this.owner.world.geometry as qf_tile_geometry;
        }

        public render_c2d_impl(ctx: CanvasRenderingContext2D): void
        {
            const tl = this.tiles.tile_size;
            ctx.strokeStyle = 'white';
            ctx.fillStyle = 'red';

            for (let x = 0; x < this.tiles.width; ++x) {
                for (let y = 0; y < this.tiles.height; ++y) {
                    if (g_draw_bounds) {
                        if (this.tiles.is_blocking(x, y)) {
                            ctx.strokeRect(x * tl, y * tl, tl, tl);
                            // this.tiles.line_trace_tile(pos, end, x, y);
                        }
                    }
                    if (g_draw_blocking_dist) {
                        ctx.fillStyle = 'red';
                        ctx.fillText(this.tiles.blocking_dist[y][x].toFixed(0), x * tl, (y + 1) * tl);
                    }

                    if (g_draw_floor_dist) {
                        ctx.fillStyle = 'green';
                        ctx.fillText(this.tiles.floor_dist[y][x].toFixed(0), (x + 0.5) * tl, (y + 1) * tl);
                    }

                    if (g_draw_jump_dist) {
                        ctx.fillStyle = 'pink';
                        ctx.fillText(this.tiles.jump_dist[y][x].toFixed(0), (x) * tl, (y + 0.5) * tl);
                    }

                    if (g_draw_id) {
                        ctx.fillStyle = 'red';
                        ctx.fillText(x.toFixed(0), (x) * tl, (y + 0.5) * tl);
                        ctx.fillText(y.toFixed(0), (x + 0.5) * tl, (y + 1) * tl);
                    }
                }
            }

            if (g_draw_bounds) {
                ctx.strokeStyle = 'pink';
                for (let act of this.owner.world.actors) {
                    for (let p of act.getcmp(qf_scene_component)) {
                        let h = qm_scale(p.bounds, 0.5);
                        let pos = p.get_pos();
                        ctx.strokeRect(pos.x - h.x, pos.y - h.y, h.x * 2, h.y * 2);
                    }
                }
            }

            ctx.strokeStyle = 'yellow';
            for (let path of qi_g_paths) {
                if (path.length == 0) continue;
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
            if (g_draw_considered)
                for (let r of considered) ctx.strokeRect(r.x, r.y, tl, tl);

            // let hits = this.tiles.d_hits;
            // for (let [p, n] of hits) ctx.fillRect(p.x - 2, p.y - 2, 4, 4);

            this.tiles.d_considered = [];
        }
    }

    // #DEBUG-END

    function spawn_bullet(world: qf_world, loc: qm_vec, dir: qm_vec, instigator: qf_actor,
        {lifespan = 0, gravity = 1000, cc = qf_cc.all, damage = 1, explosion_chance = 0}): qf_actor
    {
        let a = world.spawn_actor();

        let r = qf_attach_prim(a, new qc_anim_sprite_component(), {root: true});
        r.pos = loc;
        r.bounds = v2(8, 6);
        r.sequences['fire'] = new qr_sprite_sequence(g_character_spritesheet, [27, 28], [0.02, 1]);
        r.sequences['explode'] = new qr_sprite_sequence(g_character_spritesheet, [29, 30, qm_rnd_select(31, 32), 62], [0.03, 0.03, 0.05, 1]);
        r.play('fire');

        let p = new qc_projectile_movement();
        p.vel = dir;
        p.acc.y = gravity;
        p.lifespan = lifespan;
        p.on_hit.bind(_ => r.play('explode'), r);
        p.instigator = instigator;
        p.collision_channel = cc;
        p.damage = damage;

        p.on_hit.bind(hit => {
            if (hit.actor) {
                hit.actor.on_take_damage.broadcast(new qf_damage_event(p.damage, p.instigator, hit.normal));
            }
            if (qm_rnd() < explosion_chance) {
                spawn_explosion(p.get_world(), hit.pos);
            }
        }, a);

        qf_attach_cmp(a, p);

        return a;
    }


    function spawn_player(world: qf_world, {x, y}): qf_actor {
        let a = world.spawn_actor();
        let s = new qc_anim_sprite_component();

        let idle = s.sequences['idle'] = new qr_sprite_sequence(g_character_spritesheet, [0, 1]);
        idle.loop = true;

        s.sequences['jump'] = new qr_sprite_sequence(g_character_spritesheet, [9]);
        s.sequences['dead'] = new qr_sprite_sequence(g_character_spritesheet, [12]);
        s.sequences['falling'] = new qr_sprite_sequence(g_character_spritesheet, [0]);
        let walk = s.sequences['walk'] = new qr_sprite_sequence(g_character_spritesheet, [9 ,10, 11, 10]);
        walk.loop = true;
        walk.set_duration(0.3);

        s.play('walk');
        s.offset = v2(0, -1);
        
        qf_attach_prim(a, s, {x, y, width: 10, height: 14, coll_mask: qf_cc.player, root: true});

        let pistol = qf_attach_prim(a, new qc_sprite_component(), {x: 10, y: 0, name: 'weapon_sprite'});
        pistol.parent = s;
        pistol.flip_x = true;
        pistol.sprite = g_character_spritesheet.get_sprite(18);

        qf_attach_prim(a, new qc_label_component(), {x: 20, y: 20});

        qf_attach_cmp(a, new qc_player_movement());
        g_player_controller = qf_attach_cmp(a, new qc_player_controller());

        // #DEBUG-BEGIN
        let tdebug = new debug_draw_collisions();
        qf_attach_prim(a, tdebug, {});

        // #DEBUG-END

        world.player = a;
        return a;
    }

    function spawn_game_mode(world: qf_world, {x, y}): qf_actor {
        let a = g_world.spawn_actor();
        g_game_mode = qf_attach_cmp(a, new qc_game_mode());
        qf_attach_prim(a, new qc_label_component(), {x: g_canvas.width/4, y: g_canvas.height/4});
        return a;
    }

    function spawn_tile(actor: qf_actor, {x, y}, id: string) {
        let s = qf_attach_prim(actor, new qc_sprite_component(), {x, y, width: 10, height: 10});
        s.sprite = g_tile_spritesheet.get_sprite(9*8 + parseInt(id, 16));
    }

    function parse_level(data: string, world: qf_world): void
    {
        let geom = world.geometry;
        let x = 0, y = 0;
        let tile_map = world.spawn_actor();

        g_spawn_positions = [];
        g_item_positions  = [];
        g_npc_positions   = [];

        for (let line of data.split('\n'))
        {
            x = 0;
            for (let char of line)
            {
                let pos = qm_scale(v2(x + 0.5, y + 0.5), geom.tile_size);
                if (char === '0' || char === '1') { 
                    geom.set_blocking(x, y, true);
                    spawn_tile(tile_map, pos, char);
                }
                if (char === '@') spawn_player(world, pos)
                if (char === 's') g_spawn_positions.push(pos);
                if (char === 'n') g_npc_positions.push(pos);
                if (char ==  'i') g_item_positions.push(pos);
                x += 1;
            }
            y += 1;
        }
    }


    const enum qg_item_type {
        fire_rate_upgrade,
        bullet_range_upgrade,
        bullet_damage_upgrade,
        bullet_num_upgrade,
        bullet_explosion_upgrade,
        // bullet_knockback_upgrade,

        life_upgrade,
        coin_drop_upgrade,
        finish_shopping,
        max_none
    }

    function get_item_desc(i: qg_item_type): string {
        switch (i) {
            case qg_item_type.fire_rate_upgrade:        return 'increased fire rate';
            case qg_item_type.bullet_range_upgrade:     return 'increased bullet range';
            case qg_item_type.bullet_damage_upgrade:    return 'increased bullet damage';
            case qg_item_type.bullet_num_upgrade:       return 'increased bullet num';
            case qg_item_type.bullet_explosion_upgrade: return 'increased explosion chance';
            // case qg_item_type.bullet_knockback_upgrade: return 'increased bullet knockback';

            case qg_item_type.life_upgrade:             return 'life';
            case qg_item_type.coin_drop_upgrade:        return 'increased coin drop';
            case qg_item_type.finish_shopping:          return 'finish shopping';
        }
    }
    function get_item_cost(i: qg_item_type): number {
        switch (i) {
            case qg_item_type.fire_rate_upgrade:       return 5;
            case qg_item_type.bullet_range_upgrade:    return 5;
            case qg_item_type.bullet_damage_upgrade:   return 15;
            case qg_item_type.bullet_num_upgrade:      return 20;
            case qg_item_type.bullet_explosion_upgrade:return 5;
            // case qg_item_type.bullet_knockback_upgrade:return 80;

            case qg_item_type.life_upgrade:            return 10;
            case qg_item_type.coin_drop_upgrade:       return 10;
            case qg_item_type.finish_shopping:         return -5;
        }
    }

    let g_world: qf_world;
    let g_game_mode: qc_game_mode;
    let g_player_controller: qc_player_controller;
    let g_context: CanvasRenderingContext2D;
    let g_canvas: HTMLCanvasElement;
    export var g_time_dilation = 1;

    function tick()
    {
        g_world.tick(0.016 * g_time_dilation);

        g_context.save()
        g_context.clearRect(0, 0, g_canvas.width, g_canvas.height);
        g_context.translate(g_scene_offset.x, g_scene_offset.y);

        qr_render_w(g_context, g_world);

        g_context.restore();

        // #DEBUG-BEGIN
        qs_input.keyboard.poll_gamepad();
        // #DEBUG-END
        window.requestAnimationFrame(tick);
    }

    function reset() {
        if (g_world) {
            qu_log('score: ', g_stage);
        }
        g_stage = 0;
        g_time_dilation = 1;
        
        let t = new qf_tile_geometry(36, 18, 14);

        g_world = new qf_world();
        g_world.geometry = t;


        parse_level(
`00000000000000000000000000000000000
1                                 0
1                                 0
1 s              i             s  0
10000          11111          00000
1         1             0         0
1                                 0
1   0                         0   0
1   0                         0   0
1   0                         0   0
1   0                         0   0
1   0 si                   is 0   0
1   1010101             1010110   0
1               sis n             0
1             1010101             0
1                                 0
1  s             @             s  0
000000000000000000001111111111111111`
                    , g_world)

        spawn_game_mode(g_world, v2(0, 0));
    }

    function main()
    {
        g_canvas  = document.querySelector("#canvas");
        g_context = g_canvas.getContext("2d");
        g_context['imageSmoothingEnabled'] = false;
        g_context.scale(2, 2);

        { 
            let c = g_negative_spritesheet_image.getContext('2d');
            let {width, height} = g_negative_spritesheet_image;
            c.drawImage(g_spritesheet_image, 0, 0);
            c.fillStyle = 'white';
            c.globalCompositeOperation = 'source-in';
            c.fillRect(0, 0, width, height);
            // c.globalCompositeOperation = 'difference';
            // c.drawImage(g_spritesheet_image, 0, 0);
        }

        reset();

        qs_input.init(g_canvas);
        window.requestAnimationFrame(tick);
    }

    function load() {
        if (qr_is_img_loaded(g_spritesheet_image)) {
            main();
        } else {
            setTimeout(load, 100);
        }
    }

load();
}