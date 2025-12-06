
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with devicesFSS_V1; use devicesFSS_V1;

-- Integrantes del grupo:
-- Leonardo Chen
-- Pablo Gil
-- Mario Lorenzo
-- Zixin Zheng

-- Tareas implementadas:
-- Control de cabeceo y altitud (Task_Control_Cabeceo_Altitud)
-- Control de alabeo (Task_Control_Alabeo)
-- Control de velocidad (Task_Control_Velocidad)
-- Deteccion de obstaculos (Task_Deteccion_Obstaculos)
-- Control de display (Task_Display)
-- Control de modo automatico/manual (Task_Mode)

-- Objetos protegidos implementados:
-- Sincronizacion de datos de joystick (Pitch_Roll_Command)
-- Control de pitch de la aeronave (Pitch)
-- Control de alabeo de la aeronave (Roll)
-- Control de velocidad actual (Speed)
-- Registro de estado compartido (Status_Record)
-- Seleccion de modo automatico/manual (Selected_Mode)
-- Interrupcion del boton (Interruption_Handler)

-- NO ACTIVAR ESTE PAQUETE MIENTRAS NO SE TENGA PROGRAMADA LA INTERRUPCION
-- Packages needed to generate button interrupts       
with Ada.Interrupts.Names;
with Button_Interrupt; use Button_Interrupt;

package body fss is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- declaration of protected objects 
    -----------------------------------------------------------------------

    -- Aqui se declaran los objetos protegidos para los datos compartidos

    -- Accedido por: Task_Control_Cabeceo_Altitud (prio 11), 
    --               Task_Control_Alabeo (prio 10),
    --               Task_Control_Velocidad (prio 8).
    -- Techo (ceiling) = 11 (máxima prioridad de los llamadores).
    -- Fuente del ceiling: en el Protocolo de Techo de Prioridad de Ada, el ceiling
    -- del objeto protegido debe ser la máxima prioridad de las tareas que van a
    -- llamar a sus operaciones; por eso se toma max(11,10,8) = 11. 
    protected Pitch_Roll_Command is
      pragma Priority (11);
      procedure Get_Joystick (J: out Joystick_Samples_Type);
    end Pitch_Roll_Command;
    
    protected body Pitch_Roll_Command is
      procedure Get_Joystick (J: out Joystick_Samples_Type) is
      begin
        Read_Joystick (J);
      end Get_Joystick;
    end Pitch_Roll_Command;

    -- Accedido por: Task_Control_Cabeceo_Altitud (prio 11)
    -- Fuente del ceiling: ceiling = max(11) = 11.
    -- Techo (ceiling) = 11.
    protected Pitch is
      pragma Priority (11);
      function Get_Aircraft_Pitch return Pitch_Samples_Type;
      procedure Change_Aircraft_Pitch (P: in Pitch_Samples_Type);
    end Pitch;

    protected body Pitch is
      function Get_Aircraft_Pitch return Pitch_Samples_Type is
      begin
        return Read_Pitch;
      end Get_Aircraft_Pitch;

      procedure Change_Aircraft_Pitch (P: in Pitch_Samples_Type) is
      begin
        Set_Aircraft_Pitch (P);
      end Change_Aircraft_Pitch;
    end Pitch;

    -- Accedido por: Task_Control_Alabeo (prio 10),
    --               Task_Deteccion_Obstaculos (prio 13),
    -- Fuente del ceiling: ceiling = max(10,13) = 13.
    -- Techo (ceiling) = 13.
    protected Roll is
      pragma Priority (13);
      function Get_Aircraft_Roll return Roll_Samples_Type;
      procedure Change_Aircraft_Roll (R: in Roll_Samples_Type);
      procedure Change_Aircraft_Roll_Emergency (R: in Roll_Samples_Type);
      procedure Activate_Emergency;
      procedure Deactivate_Emergency;
    private
      Emergency_Active: Boolean := False;
    end Roll;

    protected body Roll is
      function Get_Aircraft_Roll return Roll_Samples_Type is
      begin
        return Read_Roll;
      end Get_Aircraft_Roll;

      procedure Change_Aircraft_Roll (R: in Roll_Samples_Type) is
      begin
        if not Emergency_Active then
          Set_Aircraft_Roll (R);
        end if;
      end Change_Aircraft_Roll;

      procedure Change_Aircraft_Roll_Emergency (R: in Roll_Samples_Type) is
      begin
        if Emergency_Active then
          Set_Aircraft_Roll (R);
        end if;
      end Change_Aircraft_Roll_Emergency;
      procedure Activate_Emergency is 
      begin
        Emergency_Active := True;
      end Activate_Emergency;

      procedure Deactivate_Emergency is
      begin
        Emergency_Active := False;
      end Deactivate_Emergency;
    end Roll;

    -- Accedido por: Task_Control_Velocidad (prio 8),
    --               Task_Deteccion_Obstaculos (prio 13).
    -- Fuente del ceiling: ceiling = max(8,13) = 13.
    -- Techo (ceiling) = 13.
    protected Speed is
      pragma Priority (13);
      function Get_Speed return Speed_Samples_Type;
      procedure Change_Speed (S: in Speed_Samples_Type);
    end Speed;

    protected body Speed is
      function Get_Speed return Speed_Samples_Type is
      begin
        return Read_Speed;
      end;
      procedure Change_Speed (S: in Speed_Samples_Type) is
      begin
        Set_Speed (S);
      end Change_Speed;
    end Speed;

    -- Accedido por: Task_Control_Cabeceo_Altitud (prio 11),
    --               Task_Control_Alabeo (prio 10),
    --               Task_Control_Velocidad (prio 8),
    --               Task_Deteccion_Obstaculos (prio 13).
    --               Task_Display (prio 5).
    -- Fuente del ceiling: ceiling = max(11,10,8,13,5) = 13.
    -- Techo (ceiling) = 13.
    protected Status_Record is
      pragma Priority (13);
      function Get_Altitude return Altitude_Samples_Type;
      procedure Change_Altitude (A: in Altitude_Samples_Type);
      function Get_Pilot_Power return Power_Samples_Type;
      procedure Change_Pilot_Power (P: in Power_Samples_Type);
      function Get_Speed return Speed_Samples_Type;
      procedure Change_Speed (S: in Speed_Samples_Type);
      function Get_Joystick return Joystick_Samples_Type;
      procedure Change_Joystick (J: in Joystick_Samples_Type);
      function Get_Pitch return Pitch_Samples_Type;
      procedure Change_Pitch (P: in Pitch_Samples_Type);
      function Get_Roll return Roll_Samples_Type;
      procedure Change_Roll (R: in Roll_Samples_Type);
      function Get_Message return String;
      procedure Change_Message (M: in String);
      function Get_Distance return Distance_Samples_Type;
      procedure Change_Distance (D: in Distance_Samples_Type);
      function Is_Message_Received return Boolean;
      function Is_Distance_Received return Boolean;
      procedure Mark_Message_Received;
      procedure Mark_Distance_Received;
      procedure Clear_Message_Received;
      procedure Clear_Distance_Received;
    private
      Altitude: Altitude_Samples_Type;
      Pilot_Power: Power_Samples_Type;
      Speed: Speed_Samples_Type;
      Joystick: Joystick_Samples_Type;
      Pitch: Pitch_Samples_Type;
      Roll: Roll_Samples_Type;
      Message: String (1 .. 32) := (others => ' ');
      Distance: Distance_Samples_Type;
      Message_Received: Boolean := False;
      Distance_Received: Boolean := False;
    end Status_Record;

    protected body Status_Record is
      function Get_Altitude return Altitude_Samples_Type is
      begin
        return Altitude;
      end;
      procedure Change_Altitude (A: in Altitude_Samples_Type) is
      begin
        Altitude := A;
      end;
      function Get_Pilot_Power return Power_Samples_Type is
      begin
        return Pilot_Power;
      end;
      procedure Change_Pilot_Power (P: in Power_Samples_Type) is
      begin
        Pilot_Power := P;
      end;
      function Get_Speed return Speed_Samples_Type is
      begin
        return Speed;
      end;
      procedure Change_Speed (S: in Speed_Samples_Type) is
      begin
        Speed := S;
      end;
      function Get_Joystick return Joystick_Samples_Type is
      begin
        return Joystick;
      end;
      procedure Change_Joystick (J: in Joystick_Samples_Type) is
      begin
        Joystick := J;
      end;
      function Get_Pitch return Pitch_Samples_Type is
      begin
        return Pitch;
      end;
      procedure Change_Pitch (P: in Pitch_Samples_Type) is
      begin
        Pitch := P;
      end;
      function Get_Roll return Roll_Samples_Type is
      begin
        return Roll;
      end;
      procedure Change_Roll (R: in Roll_Samples_Type) is
      begin
        Roll := R;
      end;
      function Get_Message return String is
      begin
        return Message;
      end;
      procedure Change_Message (M: in String) is
      begin
        Message := M;
      end;
      function Get_Distance return Distance_Samples_Type is
      begin
        return Distance;
      end;
      procedure Change_Distance (D: in Distance_Samples_Type) is
      begin
        Distance := D;
      end;
      function Is_Message_Received return Boolean is
      begin
        return Message_Received;
      end;
      function Is_Distance_Received return Boolean is
      begin
        return Distance_Received;
      end;
      procedure Mark_Message_Received is
      begin
        Message_Received := True;
      end;
      procedure Mark_Distance_Received is
      begin
        Distance_Received := True;
      end;
      procedure Clear_Message_Received is
      begin
        Message_Received := False;
      end;
      procedure Clear_Distance_Received is
      begin
        Distance_Received := False;
      end;
    end Status_Record;

    -- Accedido por: Task_Control_Cabeceo_Altitud (prio 11),
    --               Task_Control_Alabeo (prio 10),
    --               Task_Control_Velocidad (prio 8),
    --               Task_Deteccion_Obstaculos (prio 13).
    --               Task_Mode (prio 7).
    -- Fuente del ceiling: ceiling = max(11,10,8,13,7) = 13.
    -- Techo (ceiling) = 13.
    protected Selected_Mode is
      pragma Priority (13);
      function Is_Automatic return Boolean;
      procedure Toggle_Mode;
    private
      Automatic: Boolean := True;  -- False=Manual, True=Automatic
    end Selected_Mode;

    protected body Selected_Mode is
      function Is_Automatic return Boolean is
      begin
        return Automatic;
      end;
      procedure Toggle_Mode is
      begin
        Automatic := not Automatic;
      end;
    end Selected_Mode;

    -- Accedido por: Tarea esporádica asociada a la interrupción del botón.
    protected Interruption_Handler is
      pragma Priority (20);
      procedure Interruption;
      pragma Attach_Handler (Interruption, Ada.Interrupts.Names.External_Interrupt_2);
      entry Wait_Event;
    private
      Pending_Call : Boolean := False;
    end Interruption_Handler;

    protected body Interruption_Handler is
      procedure Interruption is
      begin
        Pending_Call := True;
      end Interruption;

      entry Wait_Event when Pending_Call is
      begin
        Pending_Call := False;
      end Wait_Event;
    end Interruption_Handler;

    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR

    task Task_Control_Cabeceo_Altitud is
        pragma Priority (11);
    end Task_Control_Cabeceo_Altitud;

    task Task_Control_Alabeo is
        pragma Priority (10);
    end Task_Control_Alabeo;

    task Task_Control_Velocidad is
        pragma Priority (8);
    end Task_Control_Velocidad;

    task Task_Deteccion_Obstaculos is
        pragma Priority (13);
    end Task_Deteccion_Obstaculos;

    task Task_Display is
        pragma Priority (5);
    end Task_Display;

    task Task_Mode is
        pragma Priority (7);
    end Task_Mode;

    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------

    -- Aqui se escriben los cuerpos de las tareas 

   task body Task_Control_Cabeceo_Altitud is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(200);

        Current_J: Joystick_Samples_Type;
        Current_A: Altitude_Samples_Type;
        Current_P: Pitch_Samples_Type;

        Target_Pitch: Pitch_Samples_Type := 0;

        Record_Update_Iteration: Integer range 0 .. 5 := 0;

        Max_Record_Update_Iterations: constant Integer := 5; -- 1000ms / 200ms = 5 iteraciones para actualizar pitch y altitud de Status_Record
        Max_Pitch: constant Pitch_Samples_Type := 30;
        Min_Pitch: constant Pitch_Samples_Type := -30;
        Margin_Upper_Pitch: constant Pitch_Samples_Type := 3;
        Margin_Lower_Pitch: constant Pitch_Samples_Type := -3;
        Low_Altitude: constant Altitude_Samples_Type := 2500;
        High_Altitude: constant Altitude_Samples_Type := 9500;
        Min_Altitude: constant Altitude_Samples_Type := 2000;
        Max_Altitude: constant Altitude_Samples_Type := 10000;
   begin
      Next_Instance := Big_Bang + Interval;
      loop
          Start_Activity ("Task_Control_Cabeceo_Altitud");  

          -- Lee Joystick del piloto y altitud de la aeronave
          Current_A := Read_Altitude;
          Pitch_Roll_Command.Get_Joystick (Current_J);
          
          -- Establece Pitch deseado en la aeronave
          Target_Pitch := Pitch_Samples_Type (Current_J(x));

          -- Se establece un margen de +3/-3º, entre los cuales la nave permanece horizontal
          -- Si pitch deseado se encuentra entre +30/-30 grados el FSS lo refleja en la posicion de la nave
          -- En modo automatico actualizar pitch de la aeronave
          if (Target_Pitch > Margin_Lower_Pitch and Target_Pitch < Margin_Upper_Pitch) then
            if Selected_Mode.Is_Automatic then
              Pitch.Change_Aircraft_Pitch (0);
            end if;
          elsif (Target_Pitch > Min_Pitch and Target_Pitch < Max_Pitch) then
            if Selected_Mode.Is_Automatic then
              Pitch.Change_Aircraft_Pitch (Target_Pitch);
            end if;
          end if;

          -- Regula si altitud sobrepasa limite de altitud baja o alta
          -- En modo automatico actualizar pitch de la aeronave
          if (Current_A < Min_Altitude or Current_A > Max_Altitude) then
            if Selected_Mode.Is_Automatic then
              Pitch.Change_Aircraft_Pitch (0);
            end if;
          end if;

          -- Alerta mediante luz 1 en caso de altitud alta o baja
          if (Current_A < Low_Altitude or Current_A > High_Altitude) then
            Light_1 (On);
          else
            Light_1 (Off);
          end if;

          -- Actualizar display de pitch y altitud
          if Record_Update_Iteration = 0 then
            -- Leer pitch de la aeronave asegurar valor real
            Current_P := Pitch.Get_Aircraft_Pitch;
            Status_Record.Change_Pitch (Current_P);
            Status_Record.Change_Altitude (Current_A);
            Record_Update_Iteration := Max_Record_Update_Iterations;
          else
            Record_Update_Iteration := Record_Update_Iteration - 1;
          end if;

          Finish_Activity ("Task_Control_Cabeceo_Altitud");
          -- Se realiza 5 veces por segundo
          delay until Next_Instance;
          Next_Instance := Next_Instance + Interval;
      end loop;
   end Task_Control_Cabeceo_Altitud;

   task body Task_Control_Alabeo is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(200);

        Current_J: Joystick_Samples_Type := (0,0);
        Current_R: Roll_Samples_Type;
        
        Target_Roll: Roll_Samples_Type; 

        Record_Update_Iteration: Integer range 0 .. 5 := 0;

        Max_Record_Update_Iterations: constant Integer := 5; -- 1000ms / 200ms = 5 iteraciones para actualizar roll de Status_Record
        Min_Roll: constant Roll_Samples_Type := -45;
        Max_Roll: constant Roll_Samples_Type := 45;
        Margin_Upper_Roll: constant Roll_Samples_Type := 3;
        Margin_Lower_Roll: constant Roll_Samples_Type := -3;
        Low_Roll: constant Roll_Samples_Type := -35;
        High_Roll: constant Roll_Samples_Type := 35;
        Warning_Message: constant String := "WARNING: HIGH ROLL ANGLE!";
   begin
      Next_Instance := Big_Bang + Interval;
      loop
          Start_Activity ("Task_Control_Alabeo");  

          -- Lee Joystick del piloto de la aeronave
          Pitch_Roll_Command.Get_Joystick (Current_J);
          
          -- Establece Roll deseado en la aeronave
          Target_Roll := Roll_Samples_Type (Current_J(y));

          -- Se establece un margen de +3/-3º, entre los cuales la nave permanece horizontal
          -- Si roll se encuentra entre +45/-45 grados el FSS lo refleja en la posicion de la nave
          -- En modo automatico actualizar roll de la aeronave
          if (Target_Roll > Margin_Lower_Roll and Target_Roll < Margin_Upper_Roll) then
            if Selected_Mode.Is_Automatic then
              Roll.Change_Aircraft_Roll (0);
            end if;
          elsif (Target_Roll > Min_Roll and Target_Roll < Max_Roll) then
            if Selected_Mode.Is_Automatic then
              Roll.Change_Aircraft_Roll (Target_Roll);
            end if;
          end if;

          -- Leer roll de la aeronave asegurar valor real
          Current_R := Roll.Get_Aircraft_Roll;

          -- Actualizar mensaje en display en caso de roll alto o bajo
          if (Current_R < Low_Roll or Current_R > High_Roll) then
            Status_Record.Change_Message (Warning_Message);
            Status_Record.Mark_Message_Received;
          end if;

          -- Actualizar display de roll
          if Record_Update_Iteration = 0 then
            Status_Record.Change_Roll (Current_R);
            Record_Update_Iteration := Max_Record_Update_Iterations;
          else
            Record_Update_Iteration := Record_Update_Iteration - 1;
          end if;

          Finish_Activity ("Task_Control_Alabeo");
          -- Se realiza 5 veces por segundo
          delay until Next_Instance;
          Next_Instance := Next_Instance + Interval;
      end loop;
   end Task_Control_Alabeo;

   task body Task_Control_Velocidad is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(300);

        Current_Pw: Power_Samples_Type := 0;
        Current_J: Joystick_Samples_Type := (0,0);
        Current_S: Speed_Samples_Type := 0;

        Calculated_S: Speed_Samples_Type := 0; 
        Input_Speed: Speed_Samples_Type := 0;
        Target_Pitch: Pitch_Samples_Type := 0;
        Target_Roll: Roll_Samples_Type := 0; 

        Record_Update_Iteration: Integer range 0 .. 3 := 0;

        Max_Record_Update_Iterations: constant Integer := 3; -- 1000ms / 300ms = 3 aproximadas iteraciones para actualizar velocidad de Status_Record
        Pitch_Roll_Additional_Speed: constant Speed_Samples_Type := 200;
        Pitch_Additional_Speed: constant Speed_Samples_Type := 150;
        Roll_Additional_Speed: constant Speed_Samples_Type := 100;
        Max_Speed: constant Speed_Samples_Type := 1000;
        Min_Speed: constant Speed_Samples_Type := 300;
    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Control_Velocidad");        
                       
            -- Lee potencia del piloto
            Read_Power (Current_Pw); 

            -- Transfiere la potencia/velocidad a la aeronave
            Calculated_S := Speed_Samples_type (float (Current_Pw) * 1.2); -- aplicar fórmula
            
            -- Lee Joystick del piloto
            Pitch_Roll_Command.Get_Joystick (Current_J);
            
            -- Establece Pitch y Roll deseado en la aeronave
            Target_Pitch := Pitch_Samples_Type (Current_J(x));
            Target_Roll := Roll_Samples_Type (Current_J(y));  

            -- Velocidad adicional en diferentes maniobras
            if (Target_Pitch /= 0 and Target_Roll /= 0) then
               Calculated_S := Calculated_S + Pitch_Roll_Additional_Speed;
            elsif (Target_Pitch /= 0) then
               Calculated_S := Calculated_S + Pitch_Additional_Speed;
            elsif (Target_Roll /= 0) then
               Calculated_S := Calculated_S + Roll_Additional_Speed;
            end if;

            -- Control alta y baja velocidad y luz 2
            if (Calculated_S > Max_Speed) then
               Input_Speed := Max_Speed;
               Light_2 (On);
            elsif (Calculated_S < Min_Speed) then
               Input_Speed := Min_Speed;
               Light_2 (On);
            else
              Input_Speed := Calculated_S;
              Light_2 (Off);
            end if;

            -- En modo automatico actualizar velocidad de la aeronave
            if Selected_Mode.Is_Automatic then
              Speed.Change_Speed (Input_Speed);
            end if;

            -- Actualizar display de velocidad, potencia del piloto y joystick
            if Record_Update_Iteration = 0 then
              -- Leer speed de la aeronave asegurar valor real
              Current_S := Speed.Get_Speed;
              Status_Record.Change_Speed (Current_S);
              Status_Record.Change_Pilot_Power (Current_Pw);
              Status_Record.Change_Joystick (Current_J);
              Record_Update_Iteration := Max_Record_Update_Iterations;
            else
              Record_Update_Iteration := Record_Update_Iteration - 1;
            end if;

            Finish_Activity ("Task_Control_Velocidad");
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Control_Velocidad;

    task body Task_Deteccion_Obstaculos is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(250);

        Current_D: Distance_Samples_Type;
        Current_L: Light_Samples_Type;
        Current_S: Speed_Samples_Type;
        Current_P: PilotPresence_Samples_Type;

        Time_Collision: Duration;
        Alarm_Time_Threshold: Duration;
        Time_Collision_Threshold: Duration;

        Emergency_Iteration: Integer := 0;
        Emergency_Active: Boolean := False;

        Max_Emergency_Iterations: constant Integer := 12; -- 3000ms / 250ms = 12 iteraciones para manterner el roll de emergencia 
        Light_Threshold: constant Light_Samples_Type := 500;
        Max_D: constant Distance_Samples_Type := 5000;
        Alarm_Time_Threshold_General: constant Duration := 10.0;
        Alarm_Time_Threshold_Bad_Conditions: constant Duration := 15.0;
        Time_Collision_Threshold_General: constant Duration := 5.0;
        Time_Collision_Threshold_Bad_Conditions: constant Duration := 10.0;
        Emergency_Roll: constant Roll_Samples_Type := 45;
    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Deteccion_Obstaculos");
            
            -- Detectar variables externas
            Read_Distance(Current_D);
            Read_Light_Intensity(Current_L);
            Current_S := Speed.Get_Speed;
            Current_P := Read_PilotPresence;
            
            -- Calcular tiempo de colision
            -- Proteger contra division por cero: si la velocidad es 0 asumimos
            -- tiempo de colision 'infinito' para evitar excepciones y que
            -- no se active la maniobra de emergencia por movimiento nulo.
            if Current_S = 0 then
              Time_Collision := Duration'Last;
            else
              Time_Collision := Duration (Float(Current_D) / Float(Current_S));
            end if;

            -- Modificar thresholds para diferentes casos
            if (Current_L < Light_Threshold or Current_P = 0) then
              Alarm_Time_Threshold := Alarm_Time_Threshold_Bad_Conditions;
              Time_Collision_Threshold := Time_Collision_Threshold_Bad_Conditions;
            else
              Alarm_Time_Threshold := Alarm_Time_Threshold_General;
              Time_Collision_Threshold := Time_Collision_Threshold_General;
            end if;

            -- Maniobra de desvio automatico
            if (Time_Collision < Time_Collision_Threshold and not Emergency_Active) then
              -- En modo automatico activar maniobra de emergencia
              if Selected_Mode.Is_Automatic then
                Roll.Activate_Emergency;
                Emergency_Active := True;
                Emergency_Iteration := 0;
              end if;
            end if;

            -- Maniobra de emergencia
            -- En modo automatico actualizar roll de la aeronave
            if (Emergency_Active) then
              -- 45 grados roll a la derecha durante 3 segundos
              if Selected_Mode.Is_Automatic then
                Roll.Change_Aircraft_Roll_Emergency (Emergency_Roll);
              end if;
              Emergency_Iteration := Emergency_Iteration + 1;
              
              if (Emergency_Iteration >= Max_Emergency_Iterations) then
                -- Estabilizar roll a 0 grados
                if Selected_Mode.Is_Automatic then
                  Roll.Change_Aircraft_Roll_Emergency (0);
                  -- Terminar maniobra de emergencia
                  Roll.Deactivate_Emergency;
                end if;
                Emergency_Active := False;
              end if;
            end if;

            -- Actualizar display de distancia de obstaculo si existe
            if (Current_D <= Max_D) then
              Status_Record.Change_Distance (Current_D);
              Status_Record.Mark_Distance_Received;
            end if;

            -- Aviso a piloto 
            if (Time_Collision < Alarm_Time_Threshold) then
              Alarm (4);
            end if;

            Finish_Activity ("Task_Deteccion_Obstaculos");
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Deteccion_Obstaculos;

    task body Task_Display is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(1000);

        Altitude: Altitude_Samples_Type := 0;
        Pilot_Power: Power_Samples_Type := 0;
        Speed: Speed_Samples_Type := 0;
        Joystick: Joystick_Samples_Type := (0,0);
        Pitch: Pitch_Samples_Type := 0;
        Roll: Roll_Samples_Type := 0;
        Message: String := "";
        Distance: Distance_Samples_Type := 0;
        Message_Received: Boolean := False;
        Distance_Received: Boolean := False;
    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Display");

            -- Lee todas las variables
            Altitude := Status_Record.Get_Altitude;
            Pilot_Power := Status_Record.Get_Pilot_Power;
            Speed := Status_Record.Get_Speed;
            Joystick := Status_Record.Get_Joystick;
            Pitch := Status_Record.Get_Pitch;
            Roll := Status_Record.Get_Roll;
            Message_Received := Status_Record.Is_Message_Received;
            Distance_Received := Status_Record.Is_Distance_Received;
            if (Message_Received) then
              Message := Status_Record.Get_Message;
            end if;
            if (Distance_Received) then
              Distance := Status_Record.Get_Distance;
            end if; 

            -- Muestra en display
            Display_Altitude (Altitude);
            Display_Pilot_Power (Pilot_Power);
            Display_Speed (Speed);
            Display_Joystick (Joystick);
            Display_Pitch (Pitch);
            Display_Roll (Roll);
            if (Message_Received) then
              Display_Message (Message);
            end if;
            if (Distance_Received) then
              Display_Distance (Distance);
            end if;

            -- Limpiar flags de mensajes y distancia recibidos despues de mostrarlo
            Status_Record.Clear_Message_Received;
            Status_Record.Clear_Distance_Received;

            Finish_Activity ("Task_Display");
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Display;

    task body Task_Mode is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(330);
    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Mode");

            Interruption_Handler.Wait_Event;

            -- Cambia el modo de vuelo
            Selected_Mode.Toggle_Mode;

            Finish_Activity ("Task_Mode");
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Mode;

    ----------------------------------------------------------------------
    ------------- procedimientos para probar los dispositivos 
    ------------- SE DEBERÁN QUITAR PARA EL PROYECTO
    ----------------------------------------------------------------------

    --quitados

begin
   Start_Activity ("Programa Principal");
   -- Tasks start automatically
   Finish_Activity ("Programa Principal");
end fss;


