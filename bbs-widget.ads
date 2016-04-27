--
--   author: Brent Seidel
--   version: V00.01
--   date: 2-Feb-2016
--
-- This file is copyrighted (C) 2016 by Brent Seidel.  It is available under
-- version 3 of the GPL.  See the file LICENSE for more details.
--
-- Please contact the author if you are interested in other licensing arrangements.
--
with Gtk.Drawing_Area;
with Cairo;
package bbs.widget is
   --
   -- This is the base package for my custom widgets.  It contains some routines
   -- common to all of my widgets.
   --
private
   --
   -- This is some utility stuff that is used by all widgets, but not really
   -- apropriate to make public.  Of course, the code is here (in the body),
   -- so you can grab it if you need it.
   --
   -- Type for RGB color.  This way we can actually define color constants.
   --
   type RGB_color is
      record
         R : Cairo.Color_Range;
         G : Cairo.Color_Range;
         B : Cairo.Color_Range;
      end record;
   --
   -- Now a procedure to set the color
   --
   procedure set_color(c : Cairo.Cairo_Context; color : RGB_color);
   --
   -- Some color constants.  Using these will help to ensure consistent colors.
   -- It will also be clearer than a list of anonymous numbers.
   --
   --
   -- Black to white
   --
   color_black : constant RGB_color := (others => 0.0);
   color_grey1 : constant RGB_color := (others => 0.1);
   color_grey2 : constant RGB_color := (others => 0.2);
   color_grey3 : constant RGB_color := (others => 0.3);
   color_grey4 : constant RGB_color := (others => 0.4);
   color_grey5 : constant RGB_color := (others => 0.5);
   color_grey6 : constant RGB_color := (others => 0.6);
   color_grey7 : constant RGB_color := (others => 0.7);
   color_grey8 : constant RGB_color := (others => 0.8);
   color_grey9 : constant RGB_color := (others => 0.9);
   color_white : constant RGB_color := (others => 1.0);
   --
   -- Named colors
   --
   color_red : constant RGB_color := (R => 1.0, others => 0.0);
   color_blue : constant RGB_color := (B => 1.0, others => 0.0);
   color_green : constant RGB_color := (G => 1.0, others => 0.0);
   color_green5 : constant RGB_color := (G => 0.5, others => 0.0);
   color_yellow : constant RGB_color := (R => 1.0, G => 1.0, B => 0.0);
   --
   -- Semantic colors
   --
   color_fail : constant RGB_color := color_red;
   color_sky : constant RGB_color := (R => 0.5, G => 0.5, B => 0.9);
   color_gnd : constant RGB_color := (R => 0.7, G => 0.6, B => 0.3);
   --
   -- This procedure centers a string of text at a particular y location.  The
   -- x location is assumed to be zero.  This matches the typical use case in
   -- the widgets.
   --
   procedure center_text(c : Cairo.Cairo_Context; y : Float; text : String);
end;
