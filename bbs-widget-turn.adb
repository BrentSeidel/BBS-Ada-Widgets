package body bbs.widget.turn is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_turn",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;
   --
   procedure gtk_new(self : in out bbs_turn) is
   begin
      self := new bbs_turn_record;
      initialize(self);
   end;
   --
   procedure initialize(self : not null access bbs_turn_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_turn'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_turn_record'class;
                  width : Integer; parent : Gdk.Gdk_Window) is
   begin
      self.value := 0.0;
      self.pointer := 0.0;
      self.size := width;
      self.slip := 0.0;
      self.slew := false;
      self.failed := False;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;
   --
   procedure set_value(self : in out bbs_turn_record'Class; value : Float) is
   begin
      if (value > 6.0) then
         self.value := 6.0;
      else
         if (value < -6.0) then
            self.value := -6.0;
         else
            self.value := value;
         end if;
      end if;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;

   procedure set_failed(self : in out bbs_turn_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;
   --
   procedure set_slip(self : in out bbs_turn_record'Class; value : Float) is
   begin
      if (value > 1.0) then
         self.slip := 1.0;
      else
         if (value < -1.0) then
            self.slip := -1.0;
         else
            self.slip := value;
         end if;
      end if;
      self.Queue_Draw;
   end;
   --
   procedure set_slew(self : in out bbs_turn_record'Class; value : Boolean; rate : Float) is
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
      me : constant bbs_turn := bbs_turn(self);
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

   function draw_turn(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_turn := bbs_turn(self);
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
         me.draw_background(context);
         Cairo.Set_Matrix(context, matrix'Access);
         draw_pointers(me, context);
      end if;
      return True;
   end;

   procedure draw_background(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      font_mat : aliased Cairo.Cairo_Matrix;
   begin
      set_color(context, color_black);
      Cairo.Paint(context);
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Get_Matrix(context, matrix'Access);
      Cairo.Get_Font_Matrix(context, font_mat'Access);
      --
      -- Draw circle
      --
      set_color(context, color_grey1);
      Cairo.Arc(context, 0.0, 0.0, 103.0, 0.0, Glib.Gdouble(two_pi));
      Cairo.Fill(context);
      --
      -- First draw the major ticks
      --
      set_color(context, color_white);
      Cairo.Set_Line_Width(context, 4.0);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Move_To(context, 105.0, 0.0);
      Cairo.Line_To(context, 125.0, 0.0);
      Cairo.Move_To(context, -105.0, 0.0);
      Cairo.Line_To(context, -125.0, 0.0);
      Cairo.Rotate(context, Glib.Gdouble(15.0*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, -105.0, 0.0);
      Cairo.Line_To(context, -125.0, 0.0);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, -Glib.Gdouble(15.0*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 105.0, 0.0);
      Cairo.Line_To(context, 125.0, 0.0);
      Cairo.Stroke(context);
      --
      -- Other labels
      --
      set_color(context, color_grey9);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Set_Font_Matrix(context, font_mat'Access);
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Set_Font_Size(context, 9.0);
      center_text(context, 15.0, "TURN COORDINATOR");
      center_text(context, 65.0, "NO PITCH");
      center_text(context, 75.0, "INFORMATION");
      --
      Cairo.Set_Font_Size(context, 12.0);
      center_text(context, 50.0, "2 MIN");
      Cairo.Move_To(context, -110.0, 50.0);
      Cairo.Show_Text(context, "L");
      Cairo.Move_To(context, 105.0, 50.0);
      Cairo.Show_Text(context, "R");
      --
      -- Draw line for slip indicator
      --
      set_color(context, color_grey7);
      Cairo.Set_Line_Cap(context, Cairo.Cairo_Line_Cap_Round);
      Cairo.Set_Line_Width(context, 12.0);
      Cairo.Move_To(context, -30.0, 30.0);
      Cairo.Line_To(context, 30.0, 30.0);
      Cairo.Stroke(context);
      --
   end;
   --
   procedure draw_pointers(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- Draw slip ball and cage
      --
      set_color(context, color_grey1);
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Move_To(context, -7.0, -24.0);
      Cairo.Line_To(context, -7.0, -36.0);
      Cairo.Move_To(context, 7.0, -24.0);
      Cairo.Line_To(context, 7.0, -36.0);
      Cairo.Stroke(context);
      --
      set_color(context, color_grey9);
      Cairo.Arc(context, -Glib.Gdouble(self.slip*30.0), -30.0, 6.0, 0.0, Glib.Gdouble(two_pi));
      Cairo.Fill(context);
      --
      -- Draw airplane symbol
      --
      Cairo.Set_Line_Width(context, 4.0);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, -Glib.Gdouble(self.pointer*5.0*Ada.Numerics.Pi/180.0));
      set_color(context, color_grey9);
      Cairo.Move_To(context, -100.0, 0.0);
      Cairo.Line_To(context, 100.0, 0.0);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, 0.0, 20.0);
      Cairo.Move_To(context, -20.0, 10.0);
      Cairo.Line_To(context, 20.0, 10.0);
      Cairo.Stroke(context);
   end;
   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context) is
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
