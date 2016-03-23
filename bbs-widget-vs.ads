--
--   author:  Brent Seidel
--   version: V00.01
--   date: 23-Mar-2016
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
package bbs.widget.vs is
   --
   -- Define a widget for a vertical speed indicator.
   --
   type bbs_vs_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_vs is access all bbs_vs_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_vs);
   procedure initialize(self : not null access bbs_vs_record'class);
   procedure setup(self : not null access bbs_vs_record'class;
                   width : Integer; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the vs to display.  It should be within
   -- the range that the vs can display.
   --
   procedure set_value(self : in out bbs_vs_record'Class; value : Float);
   --
   -- This is called when the data source for the vs has failed or is
   -- unavailable.  It replaces the vs display with a red "X".
   --
   procedure set_failed(self : in out bbs_vs_record'Class; value : Boolean);
   --
   -- This controls the slew option of the vs.  If set to false, the vs
   -- jumps to the new value.  If set to true, the vs moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_vs_record'Class; value : Boolean; rate : Float);

private
   type bbs_vs_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         major : Integer;
         minor : Integer;
         radius : Float;
         size : Integer;
         arc_start : Float;
         arc_end : Float;
         min : Float;
         max : Float;
         value : Float;
         slew_rate : Float;
         pointer : Float;
         failed : Boolean;
         park : Boolean;
         slew : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant Float := 2.0*Ada.Numerics.Pi;

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_vs(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_background(self : access bbs_vs_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_vs_record'Class; context : Cairo.Cairo_Context);
   procedure draw_pointer(Self : access bbs_vs_record'Class; context : Cairo.Cairo_Context);
   function compute_angle(Self : access bbs_vs_record'Class; value : Float) return Float;

end;
