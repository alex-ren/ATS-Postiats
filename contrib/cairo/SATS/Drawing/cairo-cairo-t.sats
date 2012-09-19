(*
** Start Time: May, 2012
** Author: Hongwei Xi (gmhwxi AT gmail DOT com)
*)

(* ****** ****** *)

staload "cairo/SATS/cairo_header.sats"

(* ****** ****** *)

/*
typedef             cairo_t;
*/

(* ****** ****** *)

/*
cairo_t *           cairo_create                        (cairo_surface_t *target);
*/
fun cairo_create
  (target: !xrsf1) : xr1 = "mac#atsctrb_cairo_create"
// end of [cairo_create]

/*
cairo_t *           cairo_reference                     (cairo_t *cr);
*/
fun cairo_reference
  {l:agz} (x: !xr l): xr (l) = "mac#atsctrb_cairo_reference"
// end of [cairo_reference]

/*
void                cairo_destroy                       (cairo_t *cr);
*/
fun cairo_destroy (x: xr1): void = "mac#atsctrb_cairo_destroy"

(* ****** ****** *)

/*
cairo_status_t      cairo_status                        (cairo_t *cr);
*/
fun cairo_status
  (ctx: !xr1) : cairo_status_t = "mac#atsctrb_cairo_status"
// end of [cairo_status]

(* ****** ****** *)

/*
void                cairo_save                          (cairo_t *cr);
*/
fun cairo_save
  {l:agz} (
  ctx: !xr l
) : (
  cairo_save_v l | void
) = "mac#atsctrb_cairo_save" // a macro

/*
void                cairo_restore                       (cairo_t *cr);
*/
fun cairo_restore
  {l:agz} (
  pf: cairo_save_v l | ctx: !xr l
) : void = "mac#atsctrb_cairo_restore" // a macro

(* ****** ****** *)

/*
cairo_surface_t *   cairo_get_target                    (cairo_t *cr);
*/
fun cairo_get0_target
  {l1:agz} (
  ctx: !xr l1
) : [l2:agz] vtget0 (xr l1, xrsf l2) = "mac#atsctrb_cairo_get0_target"

fun cairo_get1_target (ctx: !xr1): xrsf1 // for convenience

(* ****** ****** *)

/*
void                cairo_push_group                    (cairo_t *cr);
*/
fun cairo_push_group
  {l:agz} (
  ctx: !xr l
) : (
  cairo_push_group_v l | void
) = "mac#atsctrb_cairo_push_group"

/*
void                cairo_push_group_with_content       (cairo_t *cr,
                                                         cairo_content_t
							 content);
*/
fun cairo_push_group_with_content
  {l:agz} (
  ctx: !xr l, content: cairo_content_t
) : (
  cairo_push_group_v l | void
) = "mac#atsctrb_cairo_push_group_with_content"

(* ****** ****** *)

/*
cairo_pattern_t *   cairo_pop_group                     (cairo_t *cr);
*/
fun cairo_pop_group
  {l:agz} (
  pf: cairo_push_group_v l | ctx: !xr l
) : void = "mac#atsctrb_cairo_pop_group"

/*
void                cairo_pop_group_to_source           (cairo_t *cr);
*/
fun cairo_pop_group_to_source
  {l:agz} (
  pf: cairo_push_group_v l | ctx: !xr l
) : void = "mac#atsctrb_cairo_pop_group_to_source"

(* ****** ****** *)

/*
cairo_surface_t *   cairo_get_group_target              (cairo_t *cr);
*/
fun cairo_get0_group_target
  {l1:agz} (
  ctx: !xr l1
) : [l2:agz] vtget0 (xr l1, xrsf l2) = "mac#atsctrb_cairo_get0_group_target"

fun cairo_get1_group_target (ctx: !xr1): xrsf1 // for convenience

(* ****** ****** *)
/*
void                cairo_set_source_rgb                (cairo_t *cr,
                                                         double red,
                                                         double green,
                                                         double blue);
*/
fun cairo_set_source_rgb (
  ctx: !xr1, r: double, g: double, b: double
) : void = "mac#atsctrb_cairo_set_source_rgb" // end of [cairo_set_source_rgb]

/*
void                cairo_set_source_rgba               (cairo_t *cr,
                                                         double red,
                                                         double green,
                                                         double blue,
                                                         double alpha);
*/
fun cairo_set_source_rgba (
  ctx: !xr1, r: double, g: double, b: double, alpha: double
) : void = "mac#atsctrb_cairo_set_source_rgba" // end of [cairo_set_source_rgba]

/*
void                cairo_set_source                    (cairo_t *cr,
                                                         cairo_pattern_t
							 *source);
*/
fun cairo_set_source
  (ctx: !xr1, src: !xrpat1): void = "mac#atsctrb_cairo_set_source"
// end of [cairo_set_source]

(* ****** ****** *)

/*
void                cairo_set_source_surface            (cairo_t *cr,
                                                         cairo_surface_t
							 *surface,
                                                         double x,
                                                         double y);
*/
fun cairo_set_source_surface (
  ctx: !xr1, xrsf: !xrsf1, x: double, y: double
) : void = "mac#atsctrb_cairo_set_source_surface" // end of [fun]

/*
cairo_pattern_t *   cairo_get_source                    (cairo_t *cr);
*/
fun cairo_get0_source
  {l1:agz} (
  ctx: !xr l1
) : [l2:agz] vtget0 (xr l1, xrpat l2) = "mac#atsctrb_cairo_get0_source"
// end of [cairo_get0_source]

fun cairo_get1_source (ctx: !xr1): xrpat1 // for convenience

(* ****** ****** *)

/*
void                cairo_set_antialias                 (cairo_t *cr,
                                                         cairo_antialias_t
							 antialias);
*/
fun cairo_set_antialias
  (ctx: !xr1, aa: cairo_antialias_t): void = "mac#atsctrb_cairo_set_antialias"
// end of [cairo_set_antialias]

/*
cairo_antialias_t   cairo_get_antialias                 (cairo_t *cr);
*/
fun cairo_get_antialias
  (ctx: !xr1): double = "mac#atsctrb_cairo_get_antialias"
// end of [cairo_get_antialias]

(* ****** ****** *)

/*
void                cairo_set_dash                      (cairo_t *cr,
                                                         const double
							 *dashes,
                                                         int num_dashes,
                                                         double offset);
*/
(*
cairo_public void
cairo_set_dash (cairo_t      *cr,
		const double *dashes,
		int	      num_dashes,
		double	      offset);
*)
fun cairo_set_dash (
  ctx: !xr1, dashes: &double, ndashes: int, offset: double
) : void = "mac#atsctrb_cairo_set_dash"

/*
int                 cairo_get_dash_count                (cairo_t *cr);
*/

fun cairo_get_dash_count
  (ctx: !xr1): int = "mac#atsctrb_cairo_get_dash_count"
// end of [cairo_get_dash_count]

(* ****** ****** *)

/*
void                cairo_get_dash                      (cairo_t *cr,
                                                         double *dashes,
                                                         double *offset);
*/

(* ****** ****** *)

/*
void                cairo_set_fill_rule                 (cairo_t *cr,
                                                         cairo_fill_rule_t
							 fill_rule);
*/
fun cairo_set_fill_rule
  (ctx: !xr1, fr: cairo_fill_rule_t): void = "mac#atsctrb_cairo_set_fill_rule"
// end of [cairo_set_fill_rule]

/*
cairo_fill_rule_t   cairo_get_fill_rule                 (cairo_t *cr);
*/
fun cairo_get_fill_rule
  (ctx: !xr1): cairo_fill_rule_t = "mac#atsctrb_cairo_get_fill_rule"
// end of [cairo_get_fill_rule]

(* ****** ****** *)

/*
void                cairo_set_line_cap                  (cairo_t *cr,
                                                         cairo_line_cap_t
							 line_cap);
*/
fun cairo_set_line_cap
  (ctx: !xr1, lc: cairo_line_cap_t): void = "mac#atsctrb_cairo_set_line_cap"
// end of [cairo_set_line_cap]

/*
cairo_line_cap_t    cairo_get_line_cap                  (cairo_t *cr);
*/
fun cairo_get_line_cap
  (ctx: !xr1): cairo_line_cap_t = "mac#atsctrb_cairo_get_line_cap"
// end of [cairo_get_line_cap]

(* ****** ****** *)

/*
void                cairo_set_line_join                 (cairo_t *cr,
                                                         cairo_line_join_t
							 line_join);
*/
fun cairo_set_line_join
  (ctx: !xr1, lj: cairo_line_join_t): void = "mac#atsctrb_cairo_set_line_join"
// end of [cairo_set_line_join]

/*
cairo_line_join_t   cairo_get_line_join                 (cairo_t *cr);
*/
fun cairo_get_line_join
  (ctx: !xr1): cairo_line_join_t = "mac#atsctrb_cairo_get_line_join"
// end of [cairo_get_line_join]

(* ****** ****** *)

/*
void                cairo_set_line_width                (cairo_t *cr,
                                                         double width);
*/
fun cairo_set_line_width
  (ctx: !xr1, width: double): void = "mac#atsctrb_cairo_set_line_width"
// end of [cairo_set_line_width]

/*
double              cairo_get_line_width                (cairo_t *cr);
*/
fun cairo_get_line_width
  (ctx: !xr1): double = "mac#atsctrb_cairo_get_line_width"
// end of [cairo_get_line_width]

(* ****** ****** *)

/*
void                cairo_set_miter_limit               (cairo_t *cr,
                                                         double limit);
*/
fun cairo_set_miter_limit
  (ctx: !xr1, limit: double): void = "mac#atsctrb_cairo_set_miter_limit"
// end of [cairo_set_miter_limit]

/*
double              cairo_get_miter_limit               (cairo_t *cr);
*/
(*
cairo_public double
cairo_get_miter_limit (cairo_t *cr);
*)
fun cairo_get_miter_limit
  (ctx: !xr1): double = "mac#atsctrb_cairo_get_miter_limit"
// end of [cairo_get_miter_limit]

(* ****** ****** *)

/*
void                cairo_set_operator                  (cairo_t *cr,
                                                         cairo_operator_t
							 op);
*/
fun cairo_set_operator
  (ctx: !xr1, opr: cairo_operator_t): void = "mac#atsctrb_cairo_set_operator"
// end of [cairo_set_operator]

/*
cairo_operator_t    cairo_get_operator                  (cairo_t *cr);
*/
fun cairo_get_operator
  (ctx: !xr1): cairo_operator_t = "mac#atsctrb_cairo_get_operator"
// end of [cairo_get_operator]

(* ****** ****** *)

/*
void                cairo_set_tolerance                 (cairo_t *cr,
                                                         double tolerance);
*/
fun cairo_set_tolerance
  (ctx: !xr1, tolerance: double): void = "mac#atsctrb_cairo_set_tolerance"
// end of [cairo_set_tolerance]

/*
double              cairo_get_tolerance                 (cairo_t *cr);
*/
fun cairo_get_tolerance
  (ctx: !xr1): double = "mac#atsctrb_cairo_get_tolerance"
// end of [cairo_get_tolerance]

(* ****** ****** *)

/*
void                cairo_clip                          (cairo_t *cr);
*/
fun cairo_clip (ctx: !xr1): void = "mac#atsctrb_cairo_clip"

/*
void                cairo_clip_preserve                 (cairo_t *cr);
*/
fun cairo_clip_preserve (ctx: !xr1): void = "mac#atsctrb_cairo_clip_preserve"

/*
void                cairo_clip_extents                  (cairo_t *cr,
                                                         double *x1,
                                                         double *y1,
                                                         double *x2,
                                                         double *y2);
*/
fun cairo_clip_extents (
  ctx: !xr1
, x1: &double? >> double
, y1: &double? >> double
, x2: &double? >> double
, y2: &double? >> double
) : void = "mac#atsctrb_cairo_clip_extents"

/*
cairo_bool_t        cairo_in_clip                       (cairo_t *cr,
                                                         double x,
                                                         double y);
*/
fun cairo_in_clip (ctx: !xr1): bool = "mac#atsctrb_cairo_in_clip"

/*
void                cairo_reset_clip                    (cairo_t *cr);
*/
fun cairo_reset_clip (ctx: !xr1): void = "mac#atsctrb_cairo_reset_clip"

(* ****** ****** *)

/*
void                cairo_rectangle_list_destroy (cairo_rectangle_list_t *rectangle_list);
*/

/*
cairo_rectangle_list_t * cairo_copy_clip_rectangle_list (cairo_t *cr);
*/

(* ****** ****** *)

/*
void                cairo_fill                          (cairo_t *cr);
*/
fun cairo_fill (ctx: !xr1): void = "mac#atsctrb_cairo_fill"

/*
void                cairo_fill_preserve                 (cairo_t *cr);
*/
fun cairo_fill_preserve (ctx: !xr1): void = "mac#atsctrb_cairo_fill_preserve"

/*
void                cairo_fill_extents                  (cairo_t *cr,
                                                         double *x1,
                                                         double *y1,
                                                         double *x2,
                                                         double *y2);
*/
fun cairo_fill_extents (
  ctx: !xr1
, x1: &double? >> double
, y1: &double? >> double
, x2: &double? >> double
, y2: &double? >> double
) : void = "mac#atsctrb_cairo_fill_extents"

/*
cairo_bool_t        cairo_in_fill                       (cairo_t *cr,
                                                         double x,
                                                         double y);
*/
fun cairo_in_fill (ctx: !xr1): bool = "mac#atsctrb_cairo_in_fill"

(* ****** ****** *)

/*
void                cairo_mask                          (cairo_t *cr,
                                                         cairo_pattern_t *pattern);
*/

/*
void                cairo_mask_surface                  (cairo_t *cr,
                                                         cairo_surface_t *surface,
                                                         double surface_x,
                                                         double surface_y);
*/

(* ****** ****** *)

/*
void                cairo_paint                         (cairo_t *cr);
*/
fun cairo_paint (ctx: !xr1): void = "mac#atsctrb_cairo_paint"

/*
void                cairo_paint_with_alpha              (cairo_t *cr,
                                                         double alpha);
*/
fun cairo_paint_with_alpha
  (ctx: !xr1, alpha: double): void = "mac#atsctrb_cairo_paint_with_alpha"
// end of [cairo_paint_with_alpha]

(* ****** ****** *)

/*
void                cairo_stroke                        (cairo_t *cr);
*/
fun cairo_stroke (ctx: !xr1): void = "mac#atsctrb_cairo_stroke"

/*
void                cairo_stroke_preserve               (cairo_t *cr);
*/
fun cairo_stroke_preserve (ctx: !xr1): void = "mac#atsctrb_cairo_stroke_preserve"

/*
void                cairo_stroke_extents                (cairo_t *cr,
                                                         double *x1,
                                                         double *y1,
                                                         double *x2,
                                                         double *y2);
*/
fun cairo_stroke_extents (
  ctx: !xr1
, x1: &double? >> double
, y1: &double? >> double
, x2: &double? >> double
, y2: &double? >> double
) : void = "mac#atsctrb_cairo_stroke_extents"

/*
cairo_bool_t        cairo_in_stroke                     (cairo_t *cr,
                                                         double x,
                                                         double y);
*/
fun cairo_in_stroke (ctx: !xr1): bool = "mac#atsctrb_cairo_in_stroke"

(* ****** ****** *)

/*
void                cairo_copy_page                     (cairo_t *cr);
*/
fun cairo_copy_page (ctx: !xr1): void = "mac#atsctrb_cairo_copy_page"

/*
void                cairo_show_page                     (cairo_t *cr);
*/
fun cairo_show_page (ctx: !xr1): void = "mac#atsctrb_cairo_show_page"

(* ****** ****** *)

/*
unsigned int        cairo_get_reference_count           (cairo_t *cr);
*/
(*
cairo_public unsigned int
cairo_get_reference_count (cairo_t *cr);
*)
fun cairo_get_reference_count
  (ctx: !xr1): uint = "mac#atsctrb_cairo_get_reference_count"
// end of [cairo_get_reference_count]

(* ****** ****** *)

/*
cairo_status_t      cairo_set_user_data                 (cairo_t *cr,
                                                         const
							 cairo_user_data_key_t
							 *key,
                                                         void *user_data,
                                                         cairo_destroy_func_t
							 destroy);
*/
fun cairo_set_user_data (
  ctx: !xr1
, key: &cairo_user_data_key_t
, data: ptr
, fdestroy: cairo_destroy_func_t
) : cairo_status_t = "mac#atsctrb_cairo_set_user_data"
// end of [fun]

/*
void *              cairo_get_user_data                 (cairo_t *cr,
                                                         const
							 cairo_user_data_key_t *key);
*/
fun cairo_get_user_data (
  ctx: !xr1
, key: &cairo_user_data_key_t
) : ptr(*dataptr*) = "mac#atsctrb_cairo_get_user_data"
// end of [fun]

(* ****** ****** *)

(* end of [cairo-cairo-t.sats] *)