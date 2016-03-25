--
--   author:  Brent Seidel
--   version: V00.01
--   date:    24a-Mar-2016
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
package bbs.widget.airspeed is
   --
   -- Define a widget for a airspeed indicator.
   --
   type bbs_airspeed_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_airspeed is access all bbs_airspeed_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_airspeed);
   procedure initialize(self : not null access bbs_airspeed_record'class);
   procedure setup(self : not null access bbs_airspeed_record'class; minimum : float; maximum : float;
                   width : Integer; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the airspeed to display.  It should be within
   -- the range that the airspeed can display.
   --
   procedure set_value(self : in out bbs_airspeed_record'Class; value : Float);
   --
   -- This is called when the data source for the airspeed has failed or is
   -- unavailable.  It replaces the airspeed display with a red "X".
   --
   procedure set_failed(self : in out bbs_airspeed_record'Class; value : Boolean);
   --
   -- This controls the slew option of the airspeed.  If set to false, the airspeed
   -- jumps to the new value.  If set to true, the airspeed moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_airspeed_record'Class; value : Boolean; rate : Float);
   --
   -- If set_park is called with a True value, the pointer parks at the minimun
   -- and maximum limits of the airspeed.  If it is False, the pointer can move
   -- beyond the limits.
   --
   procedure set_park(self : in out bbs_airspeed_record'Class; value : Boolean);
   --
   -- This set of procedures is used to set various significant airspeeds.  These
   -- are used for certain indications on the indicator.  The airspeeds are:
   -- Vs0 - Stall speed in landing configuration (bottom of white arc)
   -- Vs1 - Stall speed in specified configuration (bottom of green arc)
   -- Vfe - Maximum flap extension speed (top of white arc)
   -- Vno - Maximum structural cruising speed (top of green and bottom of yellow arcs)
   -- Vne - Never exceed speed (top of yellow arc, red line)
   --
   procedure set_Vs0(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float);
   procedure set_Vs1(self : in out bbs_airspeed_record'Class; value : Float);
   procedure set_Vfe(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float);
   procedure set_Vno(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float);
   procedure set_Vne(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float);

private
   type bbs_airspeed_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         major : Integer;
         minor : Integer;
         radius : Float;
         size : Integer;
         arc_start : Float;
         arc_end : Float;
         Vs0 : Float;
         Vs1 : Float; -- Vs1 is always present.  It is either zero or some set value.
         Vfe : Float;
         Vno : Float;
         Vne : Float;
         min : Float;
         max : Float;
         value : Float;
         slew_rate : Float;
         pointer : Float;
         failed : Boolean;
         park : Boolean;
         slew : Boolean;
         vs0_present : Boolean;
         vfe_present : Boolean;
         Vno_present : Boolean;
         Vne_present : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant Float := 2.0*Ada.Numerics.Pi;

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_airspeed(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_airspeed_arc(self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context);
   procedure draw_ticks(self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context);
   procedure draw_pointer(Self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context);
   function compute_angle(Self : access bbs_airspeed_record'Class; value : Float) return Float;

end;
