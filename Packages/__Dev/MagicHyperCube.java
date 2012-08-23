/**
    @author Charles Kelly       Charlie.Kelly@broadband-inc.com
    Copyright (c) Charles Kelly 1998,1999
    
    This software is distributed under the GNU Public License (GPL).
    You may use this software, and re-distribute it, provided that you include
    this notice, and this source code.
    
    This software generates "Magic Squares and Hyper Cubes".
    Output is displayed in a spreadsheet (JTable) format.
    
    Magic cubes can be generated for an arbitrary order (RowSize), and for
    an arbitrary number of dimensions.

*/
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.text.*;
import java.io.*;

public class HyperMagic01 
{
    public static void main (String[] args)
    {
        JFrame frame = new MagicSizing();
        frame.show();
        
    }// public void init()
}// public class HyperMagic01 extends JApplet

/**
    The class(es) MagicSizing, IntTextDocumnet, and IntTextField support the graphical
    user interface(GUI). 
    
    The class MagicModel transfers the internal representations to the spreadsheet
    (JTable) in the GUI.
*/

class MagicSizing extends JFrame implements ActionListener
{
    JPanel SizingPanel;
    
    int RowSize, DimensionSize;
    IntTextField RowTextField, DimTextField;
    Magic_Cube mc;
    Container container;
    JTable table;
    JScrollPane js_table_holder;
    JButton createButton;
    
    int Cube_Offset_Value = 0;
    boolean default_Offset_Flag = true;
    ButtonGroup OffsetGroup;
    JRadioButton default_Button, custom_Button;
    IntTextField OffsetTextField;
    
    
    
    
    
    public MagicSizing()
    {
        setTitle ("Magic Squares and Hyper Cubes");
        setSize (900,600);
        addWindowListener (new WindowAdapter ()
            {
                public void windowClosing (WindowEvent e)
                    {
                        System.exit (0);
                    }
            });
            
        JScrollPane scrollPane;
        
        mc = new Magic_Cube (2,3,true,0);
                        
        table = new JTable (new MagicModel(mc));
        
        container = this.getContentPane();
        
        SizingPanel = new JPanel();
        RowTextField = new IntTextField (3,5);
        RowTextField.setHorizontalAlignment (SwingConstants.RIGHT);
        DimTextField = new IntTextField (2,5);
        DimTextField.setHorizontalAlignment (SwingConstants.RIGHT);
        
        SizingPanel.add(new JLabel("Row Size: ") );
        SizingPanel.add(RowTextField);
        SizingPanel.add(new JLabel("    Number of Dimensions: ") );
        SizingPanel.add(DimTextField);
        
        default_Button = new JRadioButton("Default Offset", true);
        default_Button.setActionCommand("default");
        default_Button.addActionListener(this);
        custom_Button  = new JRadioButton("Custom Offset:", false);
        custom_Button.setActionCommand("custom");
        custom_Button.addActionListener(this);
        OffsetGroup = new ButtonGroup();
        OffsetGroup.add(default_Button);
        OffsetGroup.add(custom_Button);
        
        OffsetTextField = new IntTextField (0,5);
        OffsetTextField.setHorizontalAlignment (SwingConstants.RIGHT);
        SizingPanel.add (default_Button);
        SizingPanel.add (custom_Button);
        SizingPanel.add(OffsetTextField);
        
        
        createButton = new JButton ("Make Magic");
        SizingPanel.add (createButton);
        createButton.addActionListener (this);
        
        getContentPane().add (SizingPanel, BorderLayout.NORTH);
        getContentPane().add (table.getTableHeader(), BorderLayout.CENTER);
        js_table_holder = new JScrollPane (table);
        getContentPane().add (js_table_holder);
    }// public MagicSizing()
    
    public void actionPerformed (ActionEvent evt)
    {
        String command = OffsetGroup.getSelection().getActionCommand();
        if (command.equals("default") )
        {
            default_Offset_Flag = true;
        }// if (command.equals("default") )
        else if (command.equals("custom") )
        {
            default_Offset_Flag = false;
            
            try
            {
                Cube_Offset_Value = Integer.parseInt(OffsetTextField.getText() ); 
            }// try
            catch (NumberFormatException e)
            {
                default_Offset_Flag = true;
            }// catch
        }// else if (command.equals("custom") )
        
        Object source = evt.getSource();
        if (source == createButton)
        {
            try
            {
            RowSize       = Integer.parseInt(RowTextField.getText() ); 
            DimensionSize = Integer.parseInt(DimTextField.getText() ); 
            mc = new Magic_Cube (DimensionSize, RowSize, default_Offset_Flag, Cube_Offset_Value);
               
            table = new JTable (new MagicModel(mc));
               
            table.setAutoResizeMode(table.AUTO_RESIZE_OFF);
               
            getContentPane().remove(js_table_holder);
            js_table_holder = new JScrollPane (table);
            getContentPane().add (js_table_holder);
            container.validate();
              
            }// try
            catch (NumberFormatException e)
            {
            }// catch
        }// if (source == createButton)
        
        
    SizingPanel.repaint();    
        
    }// public void actionPerformed (ActionEvent evt)
}// class MagicSizing extends JFrame implements ActionListener


/**
    The class(es) IntTextDocument and IntTextField are from:
    core JAVA 1.2
    Volume I - Fundamentals
    Cay S. Horstman, Gary Cornell
    Prentice Hall 1999
    page 394
*/
class IntTextDocument extends PlainDocument
{
    public void insertString (int offs, String str, AttributeSet a) throws BadLocationException 
    {
        if ( null == str ) return;
        
        String oldString = getText (0, getLength() );
        String newString = oldString.substring (0, offs)
                + str +oldString.substring(offs);
        try
        {
            Integer.parseInt (newString + "0" );
            super.insertString (offs, str, a);
        }
        catch (NumberFormatException e)
        {
        }
    }// public void insertString (int offs, String str, AttributeSet a)
}// class IntTextDocument extends PlainDocument

class IntTextField extends JTextField
{
    public IntTextField (int defval, int size)
    {
        super ("" + defval, size);
    }// public IntTextField (int defval, int size)
    
    public IntTextField (int size)
    {
        super (size);
    }// public IntTextField (int size)
    
    protected Document createDefaultModel()
    {
        return new IntTextDocument ();
    }// protected Document createDefaultModel()
    
}// class IntTextField extends JTextField


/**
    The class MagicModel transfer data from the internal data structure
    mc_Lookup_Table to the JTable structure in the GUI.
*/
class MagicModel  extends AbstractTableModel
{
    Magic_Cube mc;
    
    public MagicModel (Magic_Cube mcIN)
    {
        mc = mcIN;    
    }// public MagicModel (Magic_Cube mcIN)
    
    public int getRowCount()
    {
        return mc.Table_Size;
    }// public int getRowCount()
    
    public int getColumnCount()
    {
        return (1 + mc.Row_Size);
    }// public int getColumnCount()
    
    public Object getValueAt (int row, int column)
    {
        if (mc.Lookup_Table [row] < 0)
        {
            switch (column)
            {
                case 0:
                    return "****";
                default:
                    return null;
            }// switch (column)
        }
   else  switch (column)
        {
            case 0:
                return null;
            default:
                return new Integer (mc.Magic_Data [mc.Lookup_Table [row]] [column-1]);
        }// switch (column)
    }//public Object getValueAt (int row, int column)
    
    public Class getColumnClass (int column)
    {
        switch (column)
        {
            case 0:
                return String.class;
            default:
                return Integer.class;
        }// switch (column)
        
    }// public Class getColumnClass (int column)
}// class MagicModel extends AbstractTableModel 


/**
    The class Tuple is the basic data structure for each cell in a Hyper Cube.
    If a Hyper Cube has d dimensions, then each cell in the Hyper Cube is represented
    by one object of class Tuple.  Each Tuple object will contain d numbers.  
    The range for these numbers is 0..RowSize.
    RowSize is the "order" of the Hyper Cube.
*/
class Tuple
{
    private int     Tp_Size;
    private int []  Tp_data;
   
    Tuple (int Size)
    {
        Tp_Size = Size;
        Tp_data = new int [Tp_Size];
        for (int k = 0; k<Tp_Size; k++)
            {
             Tp_data [k] = 0;   
            }
        
    
		//{{INIT_CONTROLS
		//}}
	}// Tuple (int Size)
//*************************************************************************************************
    Tuple (int Size, int[]new_data)
    {                                                   // needs error checking if new_data is "too small"
        Tp_Size = Size;
        Tp_data = new int [Tp_Size];
        for (int k = 0; k<Tp_Size; k++)
            {
             Tp_data [k] = new_data[k];   
            }
        
    }// Tuple (int Size)
//*************************************************************************************************
    public void Show_Tuple ()
    {
        for (int i=0; i<Tp_Size; i++) 
        {         
         System.out.print(Tp_data[i]+ ",  ");   
        }
        System.out.println(" ");   
    }// public void Show_Tuple ()
//************************************************************************************************* 
    public void incr_Tuple (Direction_Controller dc, int Dimension, int Row_Size)
    {                                                           // needs error checking for compatible sizes
        int[] control_row = new int [Tp_Size];
        control_row = dc.get_Direction_Row(Dimension);
        
        int Temp;
        for (int k= 0; k<Tp_Size; k++)
        {
            Temp = (Tp_data[k] + control_row[k]) % Row_Size;
            
            if (Temp >= 0)
                Tp_data [k] = Temp;
            else
                Tp_data [k] = Row_Size + Temp;
        }//for (int k= 0; k<Tp_Size; k++)
    }//public void incr_Tuple (Direction_Controller dc, int Dimension)
//*************************************************************************************************
     public void decr_Tuple (Direction_Controller dc, int Dimension, int Row_Size)
    {                                                           // needs error checking for compatible sizes
        int[] control_row = new int [Tp_Size];
        control_row = dc.get_Direction_Row(Dimension);
        
        int Temp;
        for (int k= 0; k<Tp_Size; k++)
        {
            Temp = (Tp_data[k] - control_row[k]) % Row_Size;
            
            if (Temp >= 0)
                Tp_data [k] = Temp;
            else
                Tp_data [k] = Row_Size + Temp;
        }//for (int k= 0; k<Tp_Size; k++)
    }//public void decr_Tuple (Direction_Controller dc, int Dimension, int Row_Size)
//*************************************************************************************************
    public int get_Tuple_component (int index)
    {                   
        if ((index < 0) || (index>=Tp_Size))                    //needs error Throw condition
            {
                return 0;
            }
            
        return Tp_data [index];
            
    }// public int get_Tuple_component (int index)
//*************************************************************************************************
    public void set_Tuple_component (int index, int new_value)
    {                   
        if ((index < 0) || (index>=Tp_Size))                    //needs error Throw condition
            {
                return ;
            }
            
        Tp_data [index] = new_value;
            
    }// public void set_Tuple_component (int index)
//*************************************************************************************************
    public void Equate_Tuple (Tuple source)
    {
        for (int k=0; k<Tp_Size; k++)
             Tp_data[k] = source.Tp_data [k];
    }//public void Equate_Tuple (Tuple source)

//*************************************************************************************************
    public int Tuple_converter (Tuple base_Tuple)
    {
        int result = 0;
        for (int k=0; k < Tp_Size; k++)
        {
            result = result + Tp_data [k] * base_Tuple.get_Tuple_component(k);
        }// for (int k=0; k < Tp_Size; k++)
        
        return result;
    } //public int Tuple_converter (Tuple base_Tuple)
//*****************************************************************
	
}// class Tuple




/**
    Objects from the class Direction_Controller are used to increment and decrement
    Tuple objects.
*/

class Direction_Controller
{
//private class 
    private static int  Dim_count;
//private  instance    
    private int         control_matrix [] [];
    private int         MSD, LSD;               //"Most Significant Digit" & "Least Significant Digit"
//public   
//*************************************************************************************************
    public Direction_Controller (int dimensions)
    {
        Dim_count = dimensions;                                                  
        control_matrix = new int [Dim_count] [Dim_count];
        MSD =0; LSD = dimensions - 1;
        
        int Direction, test_Direction;
        for (int row_ctr = 0; row_ctr < dimensions; row_ctr++)              // go through each row of matrix
        {
            test_Direction = row_ctr % 2;
            if (1==test_Direction)
              {Direction = -1;}
            else
              {Direction = 1;}
              
            for (int col_ctr = 0; col_ctr <= row_ctr;    col_ctr++)         // set up new row with constant Direction
            {
                control_matrix [row_ctr][LSD - col_ctr] = Direction;
            }// for (int col_ctr = 0; col_ctr < row_ctr;    col_ctr++)
            
            for (int adj = row_ctr-1; adj >=0; adj --)                      // adjust 1 column in prior rows with Direction
            {
                control_matrix [adj] [LSD - row_ctr] = Direction;
            }// for (int adj = row_ctr-1; adj >=0; adj --)
            
        }// for (int row_ctr = 0; row_ctr < dimensions; row_ctr++)
        
        return;
    }//public Direction_Controller (int dimensions)
//*************************************************************************************************
    public void Show_Direction_Controller ()
    {
        for (int i=Dim_count-1; i>=0; i--) 
        {
            for (int j=0; j<Dim_count; j++)
            {
                System.out.print(control_matrix[i][j]+ ",  ");   
            }//for (j=0; j<Dim_count; j++)
            
            System.out.println(" ");   
        }// for (int i=Dim_count-1; i>=0; i--) 
        
    }// public void Show_Direction_Controller ()
    

//*************************************************************************************************
    public int get_Direction_Element (int row_num, int col_num)
    {
        return control_matrix [row_num] [col_num];     
    }//public int get_Direction_Element (int row_num, int col_num)

//*************************************************************************************************
public int[] get_Direction_Row (int row_num)
    {
        return control_matrix [row_num];     
    }//public int[] get_Direction_Row (int row_num)

//*************************************************************************************************
}// class Direction_Controller



//*************************************************************************************************
//*************************************************************************************************


/**
    A Magic_Cube object supports the internal data representation for Hyper Cubes.
    The representation is maintained in "two dimensions" to support GUI displays via spreadsheet.
*/
class Magic_Cube
{
    int                     Cube_Dimension, Row_Size, Row_Sum, Middle_Digit, Total_Rows, Cube_Offset;
    Tuple                   Start_Tuple, Middle_Tuple, Base_Converter;
    private Direction_Controller    dc;
    int [] []               Magic_Data;
    int []                  Lookup_Table;
    int                     Table_Size;     //formatted output table
    
    Magic_Cube (int Dimension, int row_length, boolean Default_Offset_Flag, int Offset_Value)
    {
        if (Dimension < 2)
            Cube_Dimension = 2;
        else    
            Cube_Dimension = Dimension;
        
     /*   if (row_length < Cube_Dimension)
            Row_Size = Cube_Dimension;
        else    
     */        
            Row_Size = row_length;
            if (0==Row_Size % 2)
                {
                  Row_Size ++;
                }
        Total_Rows  = Integer_Exponentiation (Row_Size,(Cube_Dimension-1));
        
        if (Default_Offset_Flag)
        {
            Cube_Offset = Integer_Exponentiation (Row_Size, Cube_Dimension);
            Cube_Offset --;
            Cube_Offset = Cube_Offset / 2;
        }  //if (Default_Offset_Flag)
        else
        {
            Cube_Offset = - Offset_Value;
        }// if (Default_Offset_Flag)
        
        Magic_Data = new int[Total_Rows][Row_Size];
        
        dc = new Direction_Controller (Cube_Dimension);
        calc_Middle_Tuple();
        calc_Start_Tuple();
        calc_Base_Converter();
        Fill_Magic_Data ();
        Make_Lookup_Table ();
        
        
      // try{ MagicFrame.view1.setText (3,5,"Magic Cube"); } catch (Exception e) { }
        
    }// Magic_Cube (int Dimension, int row_length, Frame1 MagicFrame)
    
//*************************************************************************************************    
    private void calc_Middle_Tuple ()
    {
        if(1==(Row_Size % 2))
            Middle_Digit = (Row_Size -1)/2;
        else
            Middle_Digit = (Row_Size   )/2;
            
        int[] Tuple_data = new int [Cube_Dimension];
        
        for (int k=0; k<Cube_Dimension; k++)
             Tuple_data[k] = Middle_Digit;
             
        Middle_Tuple = new Tuple (Cube_Dimension, Tuple_data);
        
    }// private calc_Middle_Tuple ()
//*************************************************************************************************
    private void calc_Start_Tuple()
    {
        Start_Tuple = new Tuple (Cube_Dimension);
        Start_Tuple.Equate_Tuple (Middle_Tuple);
        
        for (int dim_ctr = 0; dim_ctr < Cube_Dimension; dim_ctr++)
        {
            for (int cell_ctr = Middle_Digit-1; cell_ctr >=0; cell_ctr--)
            {
                Start_Tuple.decr_Tuple(dc, dim_ctr, Row_Size);
            }// for (int cell_ctr = Middle_Digit-1; cell_ctr >=0; cell_ctr--)
            
        }//for (int dim_ctr = 0; dim_ctr < Cube_Dimension; dim_ctr++)
        
    }// private void calc_Start_Tuple()
//*************************************************************************************************
    private void calc_Base_Converter()
    {
        int[] base_row = new int [Cube_Dimension];
        int Upper_Index = Cube_Dimension -1;
        for (int i=0; i<= Upper_Index; i++)
        {
            base_row [i] = Integer_Exponentiation (Row_Size, Upper_Index-i);
        }// for (int i=0; i<= Upper_Index; i++)
        
        Base_Converter = new Tuple (Cube_Dimension, base_row);
        
    } //private void calc_Base_Converter()
//*************************************************************************************************
    int Integer_Exponentiation (int base, int exponent)
    {
        int result = 1;
        if (0 >= exponent)
            {
                return result;
            }
            
        result = base;
        for (int k = 1; k < exponent; k++)
        {
            result = result * base;
        }// for (int k = 1; k < exponent; k++)
        
        return result;
    }// int Integer_Exponentiation (int base, int exponent)
//*************************************************************************************************
    void Make_Row (Tuple begin_Tuple, int Row_Direction, Tuple[] Tuple_Holder)
    {
        Tuple temp_Tuple = new Tuple (Cube_Dimension);
        (Tuple_Holder [0]).Equate_Tuple (begin_Tuple);
        for (int row_ctr = 1; row_ctr < Row_Size; row_ctr ++)
        {
           temp_Tuple.Equate_Tuple (Tuple_Holder [row_ctr - 1]);
           temp_Tuple.incr_Tuple (dc, Row_Direction, Row_Size);
           (Tuple_Holder [row_ctr]).Equate_Tuple (temp_Tuple);
        }// for (int row_ctr = 0; row_ctr < Row_Size; row_ctr ++)
        
    }// int[] Make_Row (Tuple begin_Tuple, int Row_Direction)
//*************************************************************************************************
    void Make_Two_Rows (Tuple begin_Tuple, int Row_Direction, Tuple[] Tuple_Holder, int[] Ten_Holder)
    {
        Tuple temp_Tuple = new Tuple (Cube_Dimension);
        (Tuple_Holder [0]).Equate_Tuple (begin_Tuple);
        Ten_Holder [0] = begin_Tuple.Tuple_converter (Base_Converter);
        
        for (int row_ctr = 1; row_ctr < Row_Size; row_ctr ++)
        {
           temp_Tuple.Equate_Tuple (Tuple_Holder [row_ctr - 1]);
           temp_Tuple.incr_Tuple (dc, Row_Direction, Row_Size);
           (Tuple_Holder [row_ctr]).Equate_Tuple (temp_Tuple);
           Ten_Holder [row_ctr] = temp_Tuple.Tuple_converter (Base_Converter);
        }// for (int row_ctr = 0; row_ctr < Row_Size; row_ctr ++)
        
    }// int[] Make_Row (Tuple begin_Tuple, int Row_Direction)
//*************************************************************************************************
    private void Fill_Magic_Data()
    {
        Tuple temp_Start_Tuple = new Tuple(Cube_Dimension);
        temp_Start_Tuple.Equate_Tuple (Start_Tuple);
        Tuple jump_Start_Tuple = new Tuple(Cube_Dimension);
        
        Tuple [] Tuple_Holder = new Tuple [Row_Size];
        for (int k=0; k<Row_Size; k++)
            {
                Tuple_Holder [k] = new Tuple (Cube_Dimension);
            }
        
        int[] Ten_Holder = new int [Row_Size];
               
        int Cell_Direction = 0;
        int Row_Counter = 0;
        
        for (int i=0; i <Total_Rows; i++)
        {
            Cell_Direction = 1;                         //Direction 0 used in rows; choose next direction
            Make_Two_Rows (temp_Start_Tuple, 0, Tuple_Holder, Ten_Holder);
  
            for (int j = 0; j<Row_Size; j++)
                {
                    Magic_Data[i][j] =  (Ten_Holder [j] - Cube_Offset);
                }// for (int j = 0; j<Row_Size; j++)
                
                
            jump_Start_Tuple.Equate_Tuple (Tuple_Holder [0]);
            Row_Counter ++;
            
            if (0 == Row_Counter % Row_Size)
            {
                for (int j=1; j <= (Cube_Dimension -1); j++)
                {
                    if (0 == Row_Counter % (Integer_Exponentiation (Row_Size, j)))
                    {
                        jump_Start_Tuple.incr_Tuple (dc, Cell_Direction, Row_Size);
                        Cell_Direction ++;
          
                       
                    }// if (0 == Row_Counter % (Integer_Exponentiation (Row_Size, j)
                }// for (int j=0; j < (Cube_Dimension -1); j++)
            }// if (0 == Row_Counter % Row_Size)
            
            if (Row_Counter < Total_Rows)
            {
                jump_Start_Tuple.incr_Tuple(dc, Cell_Direction, Row_Size);
                temp_Start_Tuple.Equate_Tuple (jump_Start_Tuple);
            }// if (Row_Count < Total_Rows)
            
                
        }//  for (int i=0; i <Total_Rows; i++)      
      
    }// private void Fill_Magic_Data()
    
    private void Make_Lookup_Table ()
    {
        int Number_of_Cells;
        int Lookup_Ctr=0, Row_Ctr=0;
        int Temp_Val;
        boolean Expon_Flag;
        
        Table_Size = 1;
        for (int i=1; i < Cube_Dimension; i++)
        {
            Table_Size += Integer_Exponentiation (Row_Size,i);
        }// for (int i=1; i <= Cube_Dimension; i++)
        Lookup_Table = new int[Table_Size];
        
        for (int j=0; (j+Lookup_Ctr)<Table_Size; j++)
        {
            Lookup_Table [j+Lookup_Ctr] = j;
            
            Temp_Val = j+1;
            if (0 == Temp_Val % (Integer_Exponentiation (Row_Size, 1)))
            {
                Lookup_Ctr ++;
                Lookup_Table [j+Lookup_Ctr] = -1;
                int k = 2;
                Expon_Flag = (0 == Temp_Val % (Integer_Exponentiation (Row_Size, k)));
                while (Expon_Flag)
                {
                    Lookup_Ctr ++;
                    Lookup_Table [j+Lookup_Ctr] = -1;
                    k++;
                    Expon_Flag = (0 == Temp_Val % (Integer_Exponentiation (Row_Size, k)));
                }// while (Expon_Flag)
            }// if (0 == Temp_Val % (Integer_Exponentiation (Row_Size, j)))
        }// for (int j=0; j<=Total_Rows; j++)
        
      int temp_debug = 0;  
    }// private void Make_Lookup_Table
}// class Magic_Cube

