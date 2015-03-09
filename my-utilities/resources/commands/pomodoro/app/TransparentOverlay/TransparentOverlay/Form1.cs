using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Timers;
using System.Windows.Forms;

namespace TransparentOverlay
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
            this.label1.Location = new System.Drawing.Point((Screen.PrimaryScreen.Bounds.Width - this.label1.Width) / 2, (Screen.PrimaryScreen.Bounds.Height - this.label1.Height) / 2);
        }

        private void closeForm()
        {
            System.Windows.Forms.Timer delayClosing = new System.Windows.Forms.Timer();
            delayClosing.Interval = 300;
            delayClosing.Tick += ((Object source, EventArgs e) => this.Close());
            delayClosing.Start();
        } 

        private void Form1_MouseClick(object sender, MouseEventArgs e)
        {
            closeForm();
        }

        private void Form1_KeyDown(object sender, KeyEventArgs e)
        {
            closeForm();
        }
    }
}
