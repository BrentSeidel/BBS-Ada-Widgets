package body bbs.widget.ai is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_ai",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;

   procedure gtk_new(self : in out bbs_ai) is
   begin
      self := new bbs_ai_record;
      initialize(self);
   end;

   procedure initialize(self : not null access bbs_ai_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_ai'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_ai_record'class;
                  width : Integer; parent : Gdk.Gdk_Window) is
   begin
      self.radius := Float(width - 50)/2.0;
      self.size := width;
      self.slew := false;
      self.failed := False;
      self.pitch := 0.0;
      self.roll := 0.0;
      self.slew_rate := 0.0;
      self.dsp_pitch := 0.0;
      self.dsp_roll := 0.0;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;

   procedure set_value(self : in out bbs_ai_record'Class; pitch : Float; roll : Float) is
   begin
      self.pitch := pitch;
      self.roll := roll;
      if (not self.slew) then
         self.dsp_pitch := self.pitch;
         self.dsp_roll := self.roll;
         self.Queue_Draw;
      end if;
   end;

   procedure set_failed(self : in out bbs_ai_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;

   procedure set_slew(self : in out bbs_ai_record'Class; value : Boolean; rate : Float) is
   begin
      self.slew_rate := rate;
      self.slew := value;
      if (value) then
         self.callback_id := self.Add_Tick_Callback(slew_handler'Access, null);
      else
         self.Remove_Tick_Callback(self.callback_id);
         self.dsp_pitch := self.pitch;
         self.dsp_roll := self.roll;
      end if;
   end;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean is
      me : constant bbs_ai := bbs_ai(self);
   begin
      if (me.dsp_pitch /= me.pitch) or (me.dsp_roll /= me.roll) then
         if (me.dsp_pitch < me.pitch) then
            if ((me.dsp_pitch - me.pitch) < me.slew_rate) then
               me.dsp_pitch := me.pitch;
            else
               me.dsp_pitch := me.dsp_pitch + me.slew_rate;
            end if;
         else
            if ((me.dsp_pitch - me.pitch) < me.slew_rate) then
               me.dsp_pitch := me.pitch;
            else
               me.dsp_pitch := me.dsp_pitch - me.slew_rate;
            end if;
         end if;
         if (me.dsp_roll < me.roll) then
            if ((me.dsp_roll - me.roll) < me.slew_rate) then
               me.dsp_roll := me.roll;
            else
               me.dsp_roll := me.dsp_roll + me.slew_rate;
            end if;
         else
            if ((me.dsp_roll - me.roll) < me.slew_rate) then
               me.dsp_roll := me.roll;
            else
               me.dsp_roll := me.dsp_roll - me.slew_rate;
            end if;
         end if;
         self.Queue_Draw;
      end if;
      return True;
   end;

   function draw_ai(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_ai := bbs_ai(self);
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      if (me.failed) then
         draw_failed(me, context);
      else
         Cairo.Translate(context, Glib.Gdouble(Float(me.size)/2.0),
                         Glib.Gdouble(Float(me.size)/2.0));
         Cairo.Get_Matrix(context, matrix'Access);
         me.draw_horizon(context);
         Cairo.Set_Matrix(context, matrix'Access);
         draw_fixed(me, context);
         Cairo.Set_Matrix(context, matrix'Access);
         draw_roll(me, context);
      end if;
      return True;
   end;
   --
   -- Draw the moving horizon.
   --
   procedure draw_horizon(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context) is
   begin
      --
      -- Translate and rotate for pitch and roll
      --
      Cairo.Rotate(context, Glib.Gdouble(self.roll*Ada.Numerics.Pi/180.0));
      Cairo.Translate(context, 0.0, Glib.Gdouble(self.pitch));
      --
      -- Draw the ground
      --
      set_color(context, color_gnd);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius*4.0), 0.0, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Fill(context);
      --
      -- Draw the sky
      --
      set_color(context, color_sky);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius*4.0), Glib.Gdouble(Ada.Numerics.Pi), Glib.Gdouble(two_pi));
      Cairo.Fill(context);
      --
      -- Draw a horizon line
      --
      Cairo.Set_Line_Width(context, 1.0);
      set_color(context, color_black);
      Cairo.Move_To(context, Glib.Gdouble(-Float(self.size)), 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
      --
      -- Draw pitch markers
      --
      Cairo.Set_Line_Width(context, 1.0);
      set_color(context, color_black);
      for x in -3 .. 3 loop
         Cairo.Move_To(context, -10.0, Glib.Gdouble(Float(x*10)));
         Cairo.Line_To(context, 10.0, Glib.Gdouble(Float(x*10)));
      end loop;
      Cairo.Stroke(context);
   end;
   --
   -- Draw the fixed parts of the attitude indicator.
   --
   procedure draw_fixed(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      --
      -- Draw roll arc
      --
      Cairo.Set_Line_Width(context, 2.0);
      set_color(context, color_white);
      Cairo.Move_To(context, Glib.Gdouble(-(self.radius)/sqrt_2), Glib.Gdouble(-(self.radius)/sqrt_2));
      Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), Glib.Gdouble(5.0*Ada.Numerics.Pi/4.0),
                Glib.Gdouble(7.0*Ada.Numerics.Pi/4.0));
      Cairo.Stroke(context);
      --
      -- Add tick marks at +/-45, +/-30, +/-15, and 0 degrees roll
      --
      Cairo.Get_Matrix(context, matrix'Access);
      Cairo.Set_Line_Width(context, 1.0);
      set_color(context, color_white);
      Cairo.Rotate(context, Glib.Gdouble((-45 + 180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble((-30 + 180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble((-15 + 180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble((180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble((15 + 180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble((30 + 180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble((45 + 180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Stroke(context);
      --
      -- Draw zero degree triangle
      --
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Set_Line_Width(context, 1.0);
      set_color(context, color_white);
      Cairo.Rotate(context, Glib.Gdouble((180)*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Line_To(context, -10.0, Glib.Gdouble(self.radius + 10.0));
      Cairo.Line_To(context, 10.0, Glib.Gdouble(self.radius + 10.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      Cairo.Stroke(context);
      --
      -- Draw a horizon line
      --
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Set_Line_Width(context, 1.0);
      set_color(context, color_white);
      Cairo.Move_To(context, Glib.Gdouble(-Float(self.size)), 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;
   --
   -- This procedure draws the roll pointer.
   --
   procedure draw_roll(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Rotate(context, Glib.Gdouble((self.roll + 180.0)*Ada.Numerics.Pi/180.0));
      Cairo.Set_Line_Width(context, 1.0);
      set_color(context, color_white);
      Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Line_To(context, -10.0, Glib.Gdouble(self.radius - 15.0));
      Cairo.Line_To(context, 10.0, Glib.Gdouble(self.radius - 15.0));
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
      Cairo.Stroke(context);
   end;
   --
   -- This procedure draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      set_color(context, color_fail);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;


end;
