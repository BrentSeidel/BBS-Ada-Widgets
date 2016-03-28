package body bbs.widget.airspeed is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_airspeed",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;

   procedure gtk_new(self : in out bbs_airspeed) is
   begin
      self := new bbs_airspeed_record;
      initialize(self);
   end;

   procedure initialize(self : not null access bbs_airspeed_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_airspeed'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_airspeed_record'class; minimum : float; maximum : float;
                  width : Integer; parent : Gdk.Gdk_Window) is
   begin
      self.min := minimum;
      self.max := maximum;
      self.value := minimum;
      self.pointer := minimum;
      self.major := integer(maximum - minimum)/10;
      self.minor := self.major*5;
      self.size := width;
      self.slew := false;
      self.failed := False;
      self.park := False;
      self.arc_start := Ada.Numerics.Pi + 0.5;
      self.arc_end := Ada.Numerics.Pi - 0.5;
      self.Vs0 := 0.0;
      self.Vs1 := 0.0;
      self.Vno := maximum;
      self.Vne := maximum;
      self.bug := 0.0;
      self.bug_state := False;
      self.vs0_present := False;
      self.vfe_present := False;
      self.Vno_present := False;
      self.Vne_present := False;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;
   --
   procedure set_value(self : in out bbs_airspeed_record'Class; value : Float) is
   begin
      self.value := value;
      if (self.park) then
         if (value < self.min) then
            self.value := self.min;
         end if;
         if (value > self.max) then
            self.value := self.max;
         end if;
      end if;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;
   --
   procedure set_Vs0(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float) is
   begin
      self.Vs0 := value;
      self.vs0_present := valid;
      self.Queue_Draw;
   end;
   --
   procedure set_vfe(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float) is
   begin
      self.Vfe := value;
      self.vfe_present := valid;
      self.Queue_Draw;
   end;
   --
   procedure set_Vs1(self : in out bbs_airspeed_record'Class; value : Float) is
   begin
      self.Vs1 := value;
      self.Queue_Draw;
   end;
   --
   procedure set_Vno(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float) is
   begin
      self.Vno := value;
      self.Vno_present := valid;
      self.Queue_Draw;
   end;
   --
   procedure set_Vne(self : in out bbs_airspeed_record'Class; valid : Boolean; value : Float) is
   begin
      self.Vne := value;
      self.Vne_present := valid;
      self.Queue_Draw;
   end;
   --
   procedure set_bug(self : in out bbs_airspeed_record'Class; state : Boolean; value : Float) is
   begin
      self.bug := value;
      self.bug_state := state;
      self.Queue_Draw;
   end;
   --
   procedure set_failed(self : in out bbs_airspeed_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;
   --
   procedure set_park(self : in out bbs_airspeed_record'Class; value : Boolean) is
   begin
      self.park := value;
      if (self.park) then
         if (self.slew) then
            if (self.value < self.min) then
               self.value := self.min;
            end if;
            if (self.value > self.max) then
               self.value := self.max;
            end if;
         else
            if (self.value < self.min) then
               self.value := self.min;
               self.pointer := self.min;
            end if;
            if (self.value > self.max) then
               self.value := self.max;
               self.pointer := self.min;
            end if;
            self.Queue_Draw;
         end if;
      end if;
   end;
   --
   procedure set_slew(self : in out bbs_airspeed_record'Class; value : Boolean; rate : Float) is
   begin
      self.slew_rate := rate;
      self.slew := value;
      if (value) then
         self.callback_id := self.Add_Tick_Callback(slew_handler'Access, null);
      else
         self.Remove_Tick_Callback(self.callback_id);
         self.pointer := self.value;
      end if;
   end;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean is
      me : constant bbs_airspeed := bbs_airspeed(self);
   begin
      if (me.pointer /= me.value) then
         if (me.pointer < me.value) then
            if ((me.value - me.pointer) < me.slew_rate) then
               me.pointer := me.value;
            else
               me.pointer := me.pointer + me.slew_rate;
            end if;
         else
            if ((me.pointer - me.value) < me.slew_rate) then
               me.pointer := me.value;
            else
               me.pointer := me.pointer - me.slew_rate;
            end if;
         end if;
         self.Queue_Draw;
      end if;
      return True;
   end;

   function draw_airspeed(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_airspeed := bbs_airspeed(self);
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      if (me.failed) then
         draw_failed(me, context);
      else
         Cairo.Translate(context, Glib.Gdouble(Float(me.size)/2.0),
                         Glib.Gdouble(Float(me.size)/2.0));
         Cairo.Scale(context, Glib.Gdouble(float(me.size) / 300.0),
                     Glib.Gdouble(float(me.size) / 300.0));
         Cairo.Get_Matrix(context, matrix'Access);
         me.draw_airspeed_arc(context);
         me.draw_ticks(context);
         Cairo.Set_Matrix(context, matrix'Access);
         draw_pointer(me, context);
      end if;
      return True;
   end;

   procedure draw_airspeed_arc(Self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      start : Glib.Gdouble;
      stop : Glib.Gdouble;
      green_start : Glib.Gdouble;
      green_stop : Glib.Gdouble;
   begin
      set_color(context, color_black);
      Cairo.Paint(context);
      Cairo.Set_Line_Width(context, 4.0);
      --
      -- White arc
      --
      if (self.vs0_present and self.vfe_present) then
         set_color(context, color_white);
         start := Glib.Gdouble(self.compute_angle(self.Vs0 - self.min));
         stop := Glib.Gdouble(self.compute_angle(self.Vfe - self.min));
         Cairo.Arc(context, 0.0, 0.0, 122.0, start, stop);
         Cairo.Stroke(context);
      end if;
      stop := Glib.Gdouble(self.arc_end + Ada.Numerics.Pi/2.0);
      --
      -- Red arc (need to change to red line at some point)
      --
      if (self.Vne_present) then
         Cairo.Get_Matrix(context, matrix'Access);
         set_color(context, color_red);
         start := Glib.Gdouble(self.compute_angle(self.Vne - self.min));
         Cairo.Rotate(context, start - Ada.Numerics.Pi/2.0);
         Cairo.Move_To(context, 0.0, 115.0);
         Cairo.Line_To(context, 0.0, 135.0);
         Cairo.Stroke(context);
         stop := start;
         Cairo.Set_Matrix(context, matrix'Access);
      end if;
      --
      -- Yellow arc
      --
      if (self.Vno_present) then
         set_color(context, color_yellow);
         start := Glib.Gdouble(self.compute_angle(self.Vno - self.min));
         Cairo.Arc(context, 0.0, 0.0, 125.0, start, stop);
         Cairo.Stroke(context);
         stop := start;
      end if;
      --
      -- Green arc for whatever is left
      --
      green_start := Glib.Gdouble(self.compute_angle(self.Vs1 - self.min));
      green_stop := stop;
      set_color(context, color_green5);
      Cairo.Arc(context, 0.0, 0.0, 125.0, green_start, green_stop);
      Cairo.Stroke(context);
      --
      -- Some text labels
      --
      set_color(context, color_white);
      Cairo.Set_Font_Size(context, 12.0);
      center_text(context, -100.0, "AIRSPEED");
      Cairo.Set_Font_Size(context, 9.0);
      center_text(context, -75.0, "MPH");
   end;

   procedure draw_ticks(Self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      ticks : Integer;
   begin
      set_color(context, color_white);
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- First draw the heading bug, if needed
      --
      if (self.bug_state) then
         set_color(context, color_grey5);
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(self.arc_start +
                      (self.bug - self.min)*(two_pi - 1.0)/(self.max - self.min)));
         Cairo.Move_To(context, 0.0, 125.0);
         Cairo.Line_To(context, -10.0, 140.0);
         Cairo.Line_To(context, 10.0, 140.0);
         Cairo.Line_To(context, 0.0, 125.0);
         Cairo.Fill(context);
      end if;
      --
      -- Then draw the major ticks
      --
      set_color(context, color_white);
      ticks := self.major;
      if (ticks > 0) then
         Cairo.Set_Line_Width(context, 2.0);
         for x in 0 .. ticks loop
            Cairo.Set_Matrix(context, matrix'Access);
            Cairo.Rotate(context, Glib.Gdouble(self.arc_start + float(x)*(two_pi - 1.0)/float(self.major)));
            Cairo.Move_To(context, 0.0, 115.0);
            Cairo.Line_To(context, 0.0, 130.0);
            center_text(context, 140.0,
                        Integer'Image(integer(self.min + float(x)*(self.max - self.min)/float(self.major))));
         end loop;
         Cairo.Stroke(context);
      end if;
      --
      -- Then draw the minor ticks
      --
      ticks := self.minor;
      if (ticks > 0) then
         Cairo.Set_Line_Width(context, 1.0);
         for x in 0 .. ticks loop
            Cairo.Set_Matrix(context, matrix'Access);
            Cairo.Rotate(context, Glib.Gdouble(self.arc_start + float(x)*(two_pi - 1.0)/float(self.minor)));
            Cairo.Move_To(context, 0.0, 120.0);
            Cairo.Line_To(context, 0.0, 125.0);
         end loop;
         Cairo.Stroke(context);
      end if;
   end;

   procedure draw_pointer(Self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Rotate(context, Glib.Gdouble(self.arc_start + (self.pointer - self.min)*(two_pi - 1.0)/(self.max - self.min)));
      set_color(context, color_white);
      Cairo.Move_To(context, 0.0, -10.0);
      Cairo.Line_To(context, 5.0, 0.0);
      Cairo.Line_To(context, 0.0, 115.0);
      Cairo.Line_To(context, -5.0, 0.0);
      Cairo.Line_To(context, 0.0, -10.0);
      Cairo.Fill(context);
   end;
   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_airspeed_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      set_color(context, color_fail);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;

   function compute_angle(Self : access bbs_airspeed_record'Class; value : Float) return Float is
   begin
      return Ada.Numerics.Pi/2.0 + self.arc_start + value*(two_pi - 1.0)/(self.max - self.min);
   end;

end;
