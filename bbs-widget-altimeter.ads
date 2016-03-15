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
package bbs.widget.altimeter is

--   author : constant String := "Brent Seidel";
--   version : constant String := "V00.01";
--   date : constant String := "15-Mar-2016";

   --
   -- Define a widget for an altimeter.  The altimeter has a number of options and not all
   -- combinations of options make sense.  In some cases unsexpected behaviour
   -- may occur.
   --
   type bbs_altimeter_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_altimeter is access all bbs_altimeter_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_altimeter);
   procedure initialize(self : not null access bbs_altimeter_record'class);
   procedure setup(self : not null access bbs_altimeter_record'class;
                   radius : Float; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the altimeter to display.  It should be within
   -- the range that the altimeter can display.
   --
   procedure set_value(self : in out bbs_altimeter_record'Class; value : Float);
   --
   -- This is called when the data source for the altimeter has failed or is
   -- unavailable.  It replaces the altimeter display with a red "X".
   --
   procedure set_failed(self : in out bbs_altimeter_record'Class; value : Boolean);
   --
   -- This controls the slew option of the altimeter.  If set to false, the altimeter
   -- jumps to the new value.  If set to true, the altimeter moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_altimeter_record'Class; value : Boolean; rate : Float);
   --
   -- Set the altimeter setting.  Note that this is display only.  The altitude
   -- calculations are done by the calling program.  This widget only does the
   -- display.  The altimeter setting is passed in as a float, converted to an
   -- integer, and then to a string.  Just because you can pass in a particular
   -- value doesn't mean that you should.
   --
   procedure altimeter_setting(self : in out bbs_altimeter_record'Class; value : Float);

private
   type bbs_altimeter_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         radius : Float;
         size : Integer;
         value : Float;
         slew_rate : Float;
         pointer : Float;
         setting : Float;
         failed : Boolean;
         slew : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant Float := 2.0*Ada.Numerics.Pi;

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_altimeter(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_Background(self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context);
   procedure draw_pointers(Self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context);
end;
