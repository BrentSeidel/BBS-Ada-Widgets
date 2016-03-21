--
--   author: Brent Seidel
--   date: 15-Mar-2016
--
-- This is a simple compass similar to the ones found in light aircraft.
--
with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics;
with Gtkada.Types;
with Gtk.Main;
with Gtkada.Builder;
with Gtk.Widget;
with Gtk.Misc;
with Gtk.Level_Bar;
with Gtk.Progress_Bar;
with Gtk.Drawing_Area;
with Gdk.Window;
with Gdk.Frame_Clock;
with Glib;
use type Glib.Gdouble;
use type Glib.Gint;
with Glib.Object;
with Glib.Properties;
with Cairo;
package bbs.widget.compass is
   --
   type bbs_compass_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_compass is access all bbs_compass_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_compass);
   procedure initialize(self : not null access bbs_compass_record'class);
   --
   -- Width is the width of the widget in pixels.  The widget is square so that
   -- only the width needs to be specified.
   --
   procedure setup(self : not null access bbs_compass_record'class;
                   width : Integer; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the compass to display.  It should be within
   -- the range that the compass can display.
   --
   procedure set_value(self : in out bbs_compass_record'Class; value : Float);
   --
   -- This is called when the data source for the compass has failed or is
   -- unavailable.  It replaces the compass display with a red "X".
   --
   procedure set_failed(self : in out bbs_compass_record'Class; value : Boolean);
   --
   -- This controls the slew option of the compass.  If set to false, the compass
   -- jumps to the new value.  If set to true, the compass moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_compass_record'Class; value : Boolean; rate : Float);
   --
   -- Sets a compass bug.  If state is true, the bug is shown.  If state is false,
   -- the bug is not shown and the value is ignored.
   --
   procedure set_bug(self : in out bbs_compass_record'Class; state : Boolean; value : Float);

private
   type bbs_compass_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         radius : Float;
         size : Integer;
         value : Float;
         slew_rate : Float;
         pointer : Float;
         bug : Float;
         bug_state : Boolean;
         failed : Boolean;
         slew : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant Float := 2.0*Ada.Numerics.Pi;

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_compass(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_Background(self : access bbs_compass_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_compass_record'Class; context : Cairo.Cairo_Context);
   procedure draw_pointers(Self : access bbs_compass_record'Class; context : Cairo.Cairo_Context);
end;
