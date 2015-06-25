#lang at-exp typed/racket

#|
The Nature of Code: Introduction: Perlin Noise with range [-0.5 0.5]
Description:        http://www.noisemachine.com/talk1/15.html
                    http://staffwww.itn.liu.se/%7Estegu/simplexnoise/simplexnoise.pdf
Author:             WarGrey Gyoudmon Ju
Maintainer:         <https://github.com/wargrey/discipline>
To-do:              4D Simplex Noise
|#

(provide (struct-out Gvector)
         (struct-out Gvector-3d)
         (struct-out Gradients)
         make-shuffle-indexes
         make-gradients
         lookfor-normalized-gradient-vector
         noise-classic
         random-simplex)

(require (for-syntax racket/list))
(require racket/fixnum)
(require math/flonum)

(struct: Gvector ([x : Flonum] [y : Flonum]))
(struct: Gvector-3d Gvector ([z : Flonum]))
(struct: (G) Gradients ([vectors : (Vectorof G)] [vmask : Index] [virtsize : (Vectorof Index)] [vsmask : Index])) 

(define: lookfor-normalized-gradient-vector : {case-> ['1D -> Flonum]
                                                      ['2D -> Gvector]
                                                      ['3D -> Gvector-3d]}
  {lambda [dimension]
    (define gradient (build-list (- (char->integer (string-ref (symbol->string dimension) 0)) 48)
                                 {lambda: ([who-cares : Index]) (fl- (fl* (random) 2.0) 1.0)}))
    (define scale (flsqrt (foldl fl+ 0.0 (map fl* gradient gradient))))
    (cond [(symbol=? dimension '1D) (car gradient)]
          [(fl< scale 1.0) (let ([g (map {lambda: ([g : Flonum]) (fl/ g (if (fl= scale 0.0) 1.0 scale))} gradient)])
                             (case dimension
                               [{2D} (Gvector (first g) (second g))]
                               [{3D} (Gvector-3d (first g) (second g) (third g))]))]
          [else (lookfor-normalized-gradient-vector dimension)])})

(define: make-shuffle-indexes : (case-> [Positive-Integer [#:double? Boolean] -> (Vectorof Index)])
  {lambda {arity #:double? [double? #true]} ; random for any input (0, ...) < n-dim-coord < (arity-1, ...) 
    (define indexes ((inst build-vector Index) arity {lambda: ([i : Index]) i}))
    (for ([index (in-range arity)])
      (define swap (vector-ref indexes index))
      (define jndex (cast (random arity) Byte))
      (vector-set! indexes index (vector-ref indexes jndex))
      (vector-set! indexes jndex swap))
    (if double? (vector-append indexes indexes) indexes)})

(define: (G) make-gradients : {case-> [(Vectorof G) -> (Gradients G)]
                                      [(Vectorof G) Positive-Index -> (Gradients G)]}
  {case-lambda [{grads} (make-gradients grads (cast (vector-length grads) Positive-Index))]
               [{grads perm-size} (define: (find-mask [upto+1 : Positive-Index]) : Index
                                    (let: check-next ([mask : Index 0] [bit-pos : Fixnum -1])
                                      (define m (fx+ mask (arithmetic-shift 1 bit-pos)))
                                      (cond [(>= m upto+1) mask]
                                            [else (check-next m (fx+ bit-pos 1))])))
                                  (Gradients grads (find-mask (cast (vector-length grads) Positive-Index)) (make-shuffle-indexes perm-size) (find-mask perm-size))]})

;(define: ensure-place-see-the-same-data-today : Pseudo-Random-Generator (current-pseudo-random-generator))

;(current-pseudo-random-generator (let*-values ([{now} (seconds->date (current-seconds))]
;                                               [{yy mm dd} (values (date-year now) (date-month now) (date-day now))]
;                                               [{trick forfun} (values (cast (string->number (format "~a~a~a" yy mm dd)) Positive-Integer)
;                                                                       (cast (string->number (format "~a~a~a" dd mm yy)) Positive-Integer))])
;                                   (vector->pseudo-random-generator (cast (vector 314159 161803 trick 299792458 66738480 forfun)
;                                                                          (Vector Positive-Integer Positive-Integer Positive-Integer
;                                                                                  Positive-Integer Positive-Integer Positive-Integer)))))

(define: /gradients/c1 : (Gradients Flonum) (make-gradients (build-vector 16 {lambda: ([wc : Index]) (lookfor-normalized-gradient-vector '1D)})))
(define: /gradients/c2 : (Gradients Gvector) (make-gradients (build-vector 16 {lambda: ([wc : Index]) (lookfor-normalized-gradient-vector '2D)})))
(define: /gradients/c3 : (Gradients Gvector-3d) (make-gradients (build-vector 16 {lambda: ([wc : Index]) (lookfor-normalized-gradient-vector '3D)})))

;(current-pseudo-random-generator ensure-place-see-the-same-data-today)

(define: /simplex/gradients/3d : (Listof (List Flonum Flonum Flonum)) (list `{1.0 1.0 0.0} `{-1.0 1.0 0.0} `{1.0 -1.0 0.0} `{-1.0 -1.0 0.0} `{1.0 0.0 1.0} `{-1.0 0.0 1.0}
                                                                            `{1.0 0.0 -1.0} `{-1.0 0.0 -1.0} `{0.0 1.0 1.0} `{0.0 -1.0 1.0} `{0.0 1.0 -1.0} `{0.0 -1.0 -1.0}))

(define: /simplex/permutation : (Listof Byte)
  ({lambda []
     (define p256 (list 151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117
                        35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41
                        55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208  89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226
                        250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167
                        43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241  81 51 145 235 249 14 239
                        107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180))
     (append p256 p256)}))


(define-syntax-rule (fweight delta) ; 6t^5-15t^4+10t^3 => ((6t-15)t+10)t^3
  (fl* delta (fl* delta (fl* delta (fl+ 10.0 (fl* delta (fl+ -15.0 (fl* 6.0 delta))))))))

(define-syntax-rule (finterpolate a b w)
  (fl+ a (fl* w (fl- b a))))

(define-syntax noise-classic
  {lambda [procedure-signature]
    (let* ([syntax=? {lambda [s1 s2] (equal? s1 (syntax->datum s2))}]
           [keywords (cdr (syntax->list procedure-signature))]
           [kw-gradients (member '#:gradients keywords syntax=?)]
           [xyz (remove* (append (if kw-gradients (take kw-gradients 2) null)) keywords)]
           [dimension (length xyz)]
           [grad (if kw-gradients (cadr kw-gradients) (case dimension [{1} #'/gradients/c1] [{2} #'/gradients/c2] [{3} #'/gradients/c3]
                                                        [else (raise-syntax-error #f "Too many arguments" procedure-signature)]))]
           [noise-classic-nd (list #''placeholder #'noise-classic-1d #'noise-classic-2d #'noise-classic-3d)])
      #`(#,(list-ref noise-classic-nd dimension) (Gradients-vectors #,grad) (Gradients-vmask #,grad) (Gradients-virtsize #,grad) (Gradients-vsmask #,grad) #,@xyz))})

(define: noise-classic-1d : {case-> [(Vectorof Flonum) Index (Vectorof Index) Index Flonum -> Flonum]}
  {lambda {gv gm perm pm Px}
    (define Qx0 (flfloor Px))
    (define Qx1 (fl+ Qx0 1.0))
    (define dx0 (fl- Px Qx0))
    (define dx1 (fl- Px Qx1))
    (define Ix0 (bitwise-and pm (fl->fx Qx0)))
    (finterpolate (fl* (vector-ref gv (bitwise-and gm (vector-ref perm Ix0))) dx0)
                  (fl* (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 1)))) dx1)
                  (fweight dx0))})

(define: noise-classic-2d : (case-> [(Vectorof Gvector) Index (Vectorof Index) Index Flonum Flonum -> Flonum])
  {lambda [gv gm perm pm Px Py]
    (define-values {Qx0 Qy0} (values (flfloor Px) (flfloor Py)))
    (define-values {Qx1 Qy1} (values (fl+ Qx0 1.0) (fl+ Qy0 1.0)))
    (define-values {dx0 dy0} (values (fl- Px Qx0) (fl- Py Qy0)))
    (define-values {dx1 dy1} (values (fl- Px Qx1) (fl- Py Qy1)))
    (define-values {Wx0 Wy0} (values (fweight dx0) (fweight dy0)))
    (define-values {Ix0 Iy0} (values (bitwise-and pm (fl->fx Qx0)) (bitwise-and pm (fl->fx Qy0))))
    (define-values {Ix1 Iy1} (values (fx+ Ix0 1) (fx+ Iy0 1)))
    (define-values {Gv00 Gv11} (values (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 (vector-ref perm Iy0)))))
                                       (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix1 (vector-ref perm Iy1)))))))
    (define-values {Gv01 Gv10} (values (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 (vector-ref perm Iy1)))))
                                       (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix1 (vector-ref perm Iy0)))))))
    (finterpolate (finterpolate (fl+ (fl* (Gvector-x Gv00) dx0) (fl* (Gvector-y Gv00) dy0)) (fl+ (fl* (Gvector-x Gv10) dx1) (fl* (Gvector-y Gv10) dy0)) Wx0)
                  (finterpolate (fl+ (fl* (Gvector-x Gv01) dx0) (fl* (Gvector-y Gv01) dy1)) (fl+ (fl* (Gvector-x Gv11) dx1) (fl* (Gvector-y Gv11) dy1)) Wx0)
                  Wy0)})

(define: noise-classic-3d : {case-> [(Vectorof Gvector-3d) Index (Vectorof Index) Index Flonum Flonum Flonum -> Flonum]}
  {lambda [gv gm perm pm Px Py Pz]
    (define-values {Qx0 Qy0 Qz0} (values (flfloor Px) (flfloor Py) (flfloor Pz)))
    (define-values {Qx1 Qy1 Qz1} (values (fl+ Qx0 1.0) (fl+ Qy0 1.0) (fl+ Qz0 1.0)))
    (define-values {dx0 dy0 dz0} (values (fl- Px Qx0) (fl- Py Qy0) (fl- Pz Qz0)))
    (define-values {dx1 dy1 dz1} (values (fl- Px Qx1) (fl- Py Qy1) (fl- Pz Qz1)))
    (define-values {Wx0 Wy0 Wz0} (values (fweight dx0) (fweight dy0) (fweight dz0)))
    (define-values {Ix0 Iy0 Iz0} (values (bitwise-and pm (fl->fx Qx0)) (bitwise-and pm (fl->fx Qy0)) (bitwise-and pm (fl->fx Qz0))))
    (define-values {Ix1 Iy1 Iz1} (values (fx+ Ix0 1) (fx+ Iy0 1) (fx+ Iz0 1)))
    (define-values {Gv000 Gv111} (values (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 (vector-ref perm (fx+ Iy0 (vector-ref perm Iz0)))))))
                                         (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix1 (vector-ref perm (fx+ Iy1 (vector-ref perm Iz1)))))))))
    (define-values {Gv001 Gv010 Gv100} (values (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 (vector-ref perm (fx+ Iy0 (vector-ref perm Iz1)))))))
                                               (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 (vector-ref perm (fx+ Iy1 (vector-ref perm Iz0)))))))
                                               (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix1 (vector-ref perm (fx+ Iy0 (vector-ref perm Iz0)))))))))
    (define-values {Gv110 Gv101 Gv011} (values (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix1 (vector-ref perm (fx+ Iy1 (vector-ref perm Iz0)))))))
                                               (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix1 (vector-ref perm (fx+ Iy0 (vector-ref perm Iz1)))))))
                                               (vector-ref gv (bitwise-and gm (vector-ref perm (fx+ Ix0 (vector-ref perm (fx+ Iy1 (vector-ref perm Iz1)))))))))
    (finterpolate (finterpolate (finterpolate (fl+ (fl* (Gvector-x Gv000) dx0) (fl+ (fl* (Gvector-y Gv000) dy0) (fl* (Gvector-3d-z Gv000) dz0)))
                                              (fl+ (fl* (Gvector-x Gv100) dx1) (fl+ (fl* (Gvector-y Gv100) dy0) (fl* (Gvector-3d-z Gv100) dz0)))
                                              Wx0)
                                (finterpolate (fl+ (fl* (Gvector-x Gv010) dx0) (fl+ (fl* (Gvector-y Gv010) dy1) (fl* (Gvector-3d-z Gv010) dz0)))
                                              (fl+ (fl* (Gvector-x Gv110) dx1) (fl+ (fl* (Gvector-y Gv110) dy1) (fl* (Gvector-3d-z Gv110) dz0)))
                                              Wx0)
                                Wy0)
                  (finterpolate (finterpolate (fl+ (fl* (Gvector-x Gv001) dx0) (fl+ (fl* (Gvector-y Gv001) dy0) (fl* (Gvector-3d-z Gv001) dz1)))
                                              (fl+ (fl* (Gvector-x Gv101) dx1) (fl+ (fl* (Gvector-y Gv101) dy0) (fl* (Gvector-3d-z Gv101) dz1)))
                                              Wx0)
                                (finterpolate (fl+ (fl* (Gvector-x Gv011) dx0) (fl+ (fl* (Gvector-y Gv011) dy1) (fl* (Gvector-3d-z Gv011) dz1)))
                                              (fl+ (fl* (Gvector-x Gv111) dx1) (fl+ (fl* (Gvector-y Gv111) dy1) (fl* (Gvector-3d-z Gv111) dz1)))
                                              Wx0)
                                Wy0)
                  Wz0)})

(define: random-simplex : {case-> [Flonum Flonum -> Flonum]}
  {case-lambda [{Px Py} (define-values {F2 G2} (values (fl* 0.5 (fl- (flsqrt 3.0) 1.0)) (fl/ (fl- 3.0 (flsqrt 3.0)) 6.0)))
                        (define Hairy-Factor (fl* (fl+ Px Py) F2))
                        (define-values {i j} (values (flfloor (fl+ Px Hairy-Factor)) (flfloor (fl+ Py Hairy-Factor))))
                        (define t (fl* G2 (fl+ i j)))
                        (define-values {X0 Y0} (values (fl- i t) (fl- j t)))
                        (define-values {x0 y0} (values (fl- Px X0) (fl- Py Y0)))
                        (define-values {i1 j1} (cond [(fl> x0 y0) (values 1 0) #| Lower triangle: (0, 0) -> (1, 0) -> (1, 1) |#]
                                                     [else (values 0 1) #| Upper triangle: (0, 0) -> (0, 1) -> (1, 1) |#]))
                        (define-values {x1 y1} (values (fl+ (fl- x0 (->fl i1)) G2) (fl+ (fl- y0 (->fl j1)) G2)))
                        (define-values {x2 y2} (values (fl+ (fl- x0 1.0) (fl* 2.0 G2)) (fl+ (fl- y0 1.0) (fl* 2.0 G2))))
                        
                        (define-values {ii jj} (values (bitwise-and #xFF (cast (fl->exact-integer i) Byte)) (bitwise-and #xFF (cast (fl->exact-integer j) Byte))))
                        (define gi0 (remainder (list-ref /simplex/permutation (+ ii (list-ref /simplex/permutation jj))) 12))
                        (define gi1 (remainder (list-ref /simplex/permutation (+ ii i1 (list-ref /simplex/permutation (+ jj j1)))) 12))
                        (define gi2 (remainder (list-ref /simplex/permutation (+ ii 1 (list-ref /simplex/permutation (+ jj 1)))) 12))
                        
                        (define-values {t0 t1 t2} (values (fl- 0.5 (fl+ (fl* x0 x0) (fl* y0 y0))) (fl- 0.5 (fl+ (fl* x1 x1) (fl* y1 y1))) (fl- 0.5 (fl+ (fl* x2 x2) (fl* y2 y2)))))
                        (fl* 35.0 (fl+ (cond [(fl< t0 0.0) 0.0]
                                             [else (define tm (fl* t0 t0))
                                                   (define gradient (list-ref /simplex/gradients/3d gi0))
                                                   (fl* tm (fl* tm (fl+ (fl* (car gradient) x0) (fl* (cadr gradient) y0))))])
                                       (fl+ (cond [(fl< t1 0.0) 0.0]
                                                  [else (define tm (fl* t1 t1))
                                                        (define gradient (list-ref /simplex/gradients/3d gi1))
                                                        (fl* tm (fl* tm (fl+ (fl* (car gradient) x1) (fl* (cadr gradient) y1))))])
                                            (cond [(fl< t2 0.0) 0.0]
                                                  [else (define tm (fl* t2 t2))
                                                        (define gradient (list-ref /simplex/gradients/3d gi2))
                                                        (fl* tm (fl* tm (fl+ (fl* (car gradient) x2) (fl* (cadr gradient) y2))))]))))]})



#|
// This method is a *lot* faster than using (int)Math.floor(x)
private static int fastfloor(double x) {
return x>0 ? (int)x : (int)x-1;
}
private static double dot(int g[], double x, double y) {
return g[0]*x + g[1]*y; }
private static double dot(int g[], double x, double y, double z) {
return g[0]*x + g[1]*y + g[2]*z; }
private static double dot(int g[], double x, double y, double z, double w) {
return g[0]*x + g[1]*y + g[2]*z + g[3]*w; }// 2D simplex noise
// 3D simplex noise
public static double noise(double xin, double yin, double zin) {
double n0, n1, n2, n3; // Noise contributions from the four corners
// Skew the input space to determine which simplex cell we're in
final double F3 = 1.0/3.0;
double s = (xin+yin+zin)*F3; // Very nice and simple skew factor for 3D
int i = fastfloor(xin+s);
int j = fastfloor(yin+s);
int k = fastfloor(zin+s);
final double G3 = 1.0/6.0; // Very nice and simple unskew factor, too
double t = (i+j+k)*G3;
double X0 = i-t; // Unskew the cell origin back to (x,y,z) space
double Y0 = j-t;
double Z0 = k-t;
double x0 = xin-X0; // The x,y,z distances from the cell origin
double y0 = yin-Y0;
double z0 = zin-Z0;
// For the 3D case, the simplex shape is a slightly irregular tetrahedron.
// Determine which simplex we are in.
int i1, j1, k1; // Offsets for second corner of simplex in (i,j,k) coords
int i2, j2, k2; // Offsets for third corner of simplex in (i,j,k) coords
if(x0>=y0) {
if(y0>=z0)
{ i1=1; j1=0; k1=0; i2=1; j2=1; k2=0; } // X Y Z order
else if(x0>=z0) { i1=1; j1=0; k1=0; i2=1; j2=0; k2=1; } // X Z Y order
else { i1=0; j1=0; k1=1; i2=1; j2=0; k2=1; } // Z X Y order
}
else { // x0<y0
if(y0<z0) { i1=0; j1=0; k1=1; i2=0; j2=1; k2=1; } // Z Y X order
else if(x0<z0) { i1=0; j1=1; k1=0; i2=0; j2=1; k2=1; } // Y Z X order
else { i1=0; j1=1; k1=0; i2=1; j2=1; k2=0; } // Y X Z order
}
//
//
//
//
A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
c = 1/6.double
double
double
double
double
double
double
double
double
x1 = x0 - i1 + G3; // Offsets for second corner in (x,y,z) coords
y1 = y0 - j1 + G3;
z1 = z0 - k1 + G3;
x2 = x0 - i2 + 2.0*G3; // Offsets for third corner in (x,y,z) coords
y2 = y0 - j2 + 2.0*G3;
z2 = z0 - k2 + 2.0*G3;
x3 = x0 - 1.0 + 3.0*G3; // Offsets for last corner in (x,y,z) coords
y3 = y0 - 1.0 + 3.0*G3;
z3 = z0 - 1.0 + 3.0*G3;
// Work out the hashed gradient indices of the four simplex corners
int ii = i & 255;
int jj = j & 255;
int kk = k & 255;
int gi0 = perm[ii+perm[jj+perm[kk]]] % 12;
int gi1 = perm[ii+i1+perm[jj+j1+perm[kk+k1]]] % 12;
int gi2 = perm[ii+i2+perm[jj+j2+perm[kk+k2]]] % 12;
int gi3 = perm[ii+1+perm[jj+1+perm[kk+1]]] % 12;
// Calculate the contribution from the four corners
double t0 = 0.6 - x0*x0 - y0*y0 - z0*z0;
if(t0<0) n0 = 0.0;
else {
t0 *= t0;
n0 = t0 * t0 * dot(grad3[gi0], x0, y0, z0);
}
double t1 = 0.6 - x1*x1 - y1*y1 - z1*z1;
if(t1<0) n1 = 0.0;
else {
t1 *= t1;
n1 = t1 * t1 * dot(grad3[gi1], x1, y1, z1);
}
double t2 = 0.6 - x2*x2 - y2*y2 - z2*z2;
if(t2<0) n2 = 0.0;
else {
t2 *= t2;
n2 = t2 * t2 * dot(grad3[gi2], x2, y2, z2);
}
double t3 = 0.6 - x3*x3 - y3*y3 - z3*z3;
if(t3<0) n3 = 0.0;
else {
t3 *= t3;
n3 = t3 * t3 * dot(grad3[gi3], x3, y3, z3);
}
}
// Add contributions from each corner to get the final noise value.
// The result is scaled to stay just inside [-1,1]
return 32.0*(n0 + n1 + n2 + n3);
|#