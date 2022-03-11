# Dashboard_RCI

Most of the content in the dashboard can visualize properly without any previous step, however, for the line charts related to the last two tasks of the test to properly display, it is necessary that you first follow some steps.

Steps:
1. This charts use R visualization, so it is necessary that you install R in your computer. Here is a link where it can be downloaded for free https://cran.r-project.org/mirrors.html
2. After properly installing R, it is necessary that you run the "preparation" R script that you can find in this repository. This script is necessary for installing the "ggplot2" library on your computer, as this is used for rendering the plots displayed in the last two sections of the dashboard.
3. After downloading and opening the R script, in the first line of the script, there will be a simple command that read "install.packages("ggplot2")", position yourself on this line and press ctrl+enter on your keyboard. If you are using base R, a list with CRAN mirrors will display, pick one and let the program install the library. If you are using R Studio, the installing process will begin inmediately.
4. After following these steps you can open the Power BI file and everything should display appropriately. Keep in mind that you will need Power BI Desktop on your computer to visualize the dashboard.

Aside from this, the R script file contains other steps that have been taken for the construction of the dashboards. The script is commented, so it is possible to see directly what my ideas have been during the process. Feel free to ask about any step that does not seem clear.
