#preamble----
{
  #     code written by Jarred C Lloyd, orcid: 0000-0002-4429-873X, https://www.researchgate.net/profile/Jarred-Lloyd
  #     last updated 2022-01-15
  #
  #     Copyright (C) <2022>  <Jarred C. Lloyd>
  #
  #     This program is free software: you can redistribute it and/or modify
  #     it under the terms of the GNU General Public License as published by
  #     the Free Software Foundation, either version 3 of the License, or
  #     (at your option) any later version.
  #
  #     This program is distributed in the hope that it will be useful,
  #     but WITHOUT ANY WARRANTY; without even the implied warranty of
  #     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #     GNU General Public License for more details.
  #
  #     You should have received a copy of the GNU General Public License
  #     along with this program.  If not, see <https://www.gnu.org/licenses/>.

  # colour values utilise the scico package for colour-blind safe palettes (see Crameri et al. 2020, doi:10.1038/s41467-020-19160-7 and https://github.com/thomasp85/scico)
  # use scico::scico_palette_show() to view available colour palettes in the package. Code can easily be modified to use alternate colour packages, base colours or custom colour mapping.
  }

#plotting code----
{
  #load library, install by un-commenting next line (remove hashtag) if not already installed----
  #install.packages(c('tidyverse','patchwork','scico','provenance'))
  {
    library(tidyverse)
    library(patchwork)
    library(scico)
    library(provenance)
  }
  
  #set working directory (path to read file from and save exports etc)----
  setwd("D:/")
  
  #load data, if loading file from place other than working directory use full file path and name----
  TEdata <- read_csv("dataset_sample.csv")
  
  #data preparation (Shannon Radii, calculations, and clean up)----
  {
    #Shannon radii vectors
    {
      rlanth_sh_radii <- {
        c(
          1.160,
          1.143,
          1.126,
          1.109,
          1.093,
          1.079,
          1.066,
          1.053,
          1.040,
          1.027,
          1.015,
          1.004,
          0.994,
          0.985,
          0.977
        )
      }
      rlanth_sh_label <- {
        c(
          'La',
          'Ce',
          'Pr',
          'Nd',
          'Pm',
          'Sm',
          'Eu',
          'Gd',
          'Tb',
          'Dy',
          'Ho',
          'Er',
          'Tm',
          'Yb',
          'Lu'
        )
      }
    }
    #calculate U/Yb,Yb/U, U/Th, Lu/Nd, traditional Ce/Ce*, and traditional Eu/Eu* (Ce* and Eu* normalised to CI chondrite O'Neill 2016)----
    #Ce_MFR and Eu_MFR are the Ce* and Eu* anomalies calculated by BLambdaR
    {
      TEdata <- {
        mutate(
          TEdata,
          UYb = U238 / Yb172,
          YbU = Yb172 / U238,
          ThU = Th232 / U238,
          LuNd = Lu175 / Nd146,
          CeAn = (Ce140 / 0.6308) / sqrt((La139 / 0.2472) * (Pr141 /
                                                               0.095)),
          EuAn = (Eu153 / 0.0592) / sqrt((Sm147 / 0.1542) * (Gd157 /
                                                               0.2059))
          
        )
      }
    }
    #clear infinity values
    {
      TEdata <- {
        do.call(data.frame, lapply(TEdata, function(x)
          replace(x, is.infinite(x), NA)))
      }
      TEdataU <- {
        drop_na(TEdata, U238)
      }
      TEdataCe <- {
        drop_na(TEdata, Ce_MFR)
      }
      TEdataEu <- {
        drop_na(TEdata, Eu_MFR)
      }
    }
    #calculate REE CI-chondrite (O'Neill 2016) normalised, to plot against Shannon Radii----
    {
      CINorm <- {
        transmute(
          TEdata,
          "1.160" = La139 / 0.2472,
          "1.143" = Ce140 / 0.6308,
          "1.126" = Pr141 / 0.0950,
          "1.109" = Nd146 / 0.4793,
          "1.079" = Sm147 / 0.1542,
          "1.066" = Eu153 / 0.0592,
          "1.053" = Gd157 / 0.2059,
          "1.040" = Tb159 / 0.0375,
          "1.027" = Dy163 / 0.2540,
          "1.015" = Ho165 / 0.0554,
          "1.004" = Er166 / 0.1645,
          "0.994" = Tm169 / 0.0258,
          "0.985" = Yb172 / 0.1684,
          "0.977" = Lu175 / 0.0251
        )
      }
    }
    #remove values of 0----
    {
      CINorm[CINorm == 0] <- NA
    }
    #pivot CINorm in long form, transform col names to numeric values----
    {
      CINormPiv <- {
        pivot_longer(
          CINorm,
          everything(),
          names_to = "rLanth",
          names_transform = list(rLanth = as.numeric)
        )
      }
      CINormPiv <- {
        mutate(
          CINormPiv,
          lanth_symbol = case_when(
            rLanth == 1.160 ~ 'La',
            rLanth == 1.143 ~ 'Ce',
            rLanth == 1.126 ~ 'Pr',
            rLanth == 1.109 ~ 'Nd',
            rLanth == 1.079 ~ 'Sm',
            rLanth == 1.066 ~ 'Eu',
            rLanth == 1.053 ~ 'Gd',
            rLanth == 1.040 ~ 'Tb',
            rLanth == 1.027 ~ 'Dy',
            rLanth == 1.015 ~ 'Ho',
            rLanth == 1.004 ~ 'Er',
            rLanth == 0.994 ~ 'Tm',
            rLanth == 0.985 ~ 'Yb',
            rLanth == 0.977 ~ 'Lu'
          )
        )
      }
    }
  }
  #plot generation----
  {
    #"Continental" vs "oceanic" derived zircon plot (Grimes et al. 2007)----
    {
      #oceanic vs continental U/Yb rule data----
      {
        ruleUYb <-
          data.frame(
            x1 = 200,
            x2 = 100000,
            y1 = 0.01,
            y2 = 5
          )
      }
      #plot U/Yb against Y, scale to (x,y) (10,0.01 : 100000, 100)
      {
        pUYb_Y  <-
          ggplot(TEdata,
                 aes(Y89, UYb)) +
          geom_point(
            size = 2,
            alpha = 0.75,
            shape = 16,
            na.rm = TRUE,
            aes(colour = FilteredAge)
          ) +
          scale_colour_scico(palette = "acton", direction = 1) +
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#FFFFFF"),
            legend.position = c(0.90, 0.20)
          ) +
          scale_x_log10(
            labels = scales::label_number(),
            expand = c(0, 0),
            limits = c(10, 100000)
          ) +
          scale_y_log10(
            limits = c(0.01, 100),
            labels = scales::label_number(),
            expand = c(0, 0)
          ) +
          annotation_logticks() +
          geom_segment(data = ruleUYb , aes(
            x = x1,
            y = y1,
            xend = x2,
            yend = y2
          )) +
          labs(
            x = expression("Log"[10] * "(" ^ 89 * "Y)"),
            y = expression("Log"[10] * "(" ^ 238 * "U/" ^ 172 * "Yb)"),
            colour = "Filtered Age (Ma)",
            title = "Continental vs oceanic derived zircon plot"
          )
      }
    }
    #plot Yb/U, Ce_MFR, Eu_MFR, & lamdba 1-3, against age, change limits to suit data (format as c(min, max)), based on plots of Verdel et al. 2020----
    {
      #Scatterplots with 2d density (bandwidth of density chosen by botev algorithm implemented in the provenance package---
      {
        pYbU_scatter <- {
          ggplot(TEdataU,
                 aes(FilteredAge, YbU)) +
            scale_fill_scico(palette = 'acton',
                             direction = -1) +
            stat_density_2d(
              aes(fill = after_stat(level)),
              bins = 50,
              geom = 'polygon',
              show.legend = FALSE,
              na.rm = TRUE,
              contour_var = 'ndensity',
              h = c(
                provenance::botev(TEdataU$FilteredAge),
                provenance::botev(log10(TEdataU$YbU))
              )
            ) +
            geom_point(
              size = 0.75,
              alpha = 0.5,
              na.rm = TRUE,
              aes(shape = Sample)
            ) +
            xlab(NULL) +
            scale_y_log10(breaks = scales::breaks_log(),
                          labels = scales::label_number()) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            coord_cartesian(clip = "off") +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression("Log"[10] * " (" ^ 172 * "Yb/" ^ 238 * "U)")) +
            guides(col = guide_legend(ncol = 1))
        }
        pCeAn_scatter <- {
          ggplot(TEdataCe,
                 aes(FilteredAge, Ce_MFR)) +
            scale_fill_scico(palette = 'acton',
                             direction = -1) +
            stat_density_2d(
              aes(fill = after_stat(level), alpha = after_stat(level)),
              bins = 50,
              geom = 'polygon',
              contour_var = 'ndensity',
              h = c(
                provenance::botev(TEdataCe$FilteredAge),
                provenance::botev(log10(TEdataCe$Ce_MFR))
              ),
              show.legend = FALSE,
              na.rm = TRUE
            ) +
            geom_point(
              size = 0.75,
              alpha = 0.5,
              na.rm = TRUE,
              aes(shape = Sample)
            ) +
            xlab(NULL) +
            scale_y_log10(breaks = scales::breaks_log(),
                          labels = scales::label_number()) +
            annotation_logticks(
              sides = "l",
              short = unit(-0.1, "cm"),
              mid = unit(-0.2, "cm"),
              long = unit(-0.3, "cm")
            ) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression("Log"[10] * "(Ce" ^ "*" * ")")) +
            guides(col = guide_legend(ncol = 1))
        }
        pEuAn_scatter <- {
          ggplot(TEdataEu,
                 aes(FilteredAge, Eu_MFR)) +
            scale_fill_scico(palette = 'acton',
                             direction = -1) +
            stat_density_2d(
              aes(fill = after_stat(level), alpha = after_stat(level)),
              bins = 50,
              geom = 'polygon',
              contour_var = 'ndensity',
              h = c(
                provenance::botev(TEdataEu$FilteredAge),
                provenance::botev(log10(TEdataEu$Eu_MFR))
              ),
              show.legend = FALSE,
              na.rm = TRUE
            ) +
            geom_point(
              size = 0.75,
              alpha = 0.5,
              na.rm = TRUE,
              aes(shape = Sample)
            ) +
            scale_y_continuous(
              trans = "log10",
              breaks = scales::breaks_log(),
              labels = scales::label_number()
            ) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            xlab(NULL) +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression("Log"["10"] * "(Eu" ^ "*" * ")")) +
            guides(col = guide_legend(ncol = 1))
        }
        pl1vAge <- {
          ggplot(TEdata,
                 aes(FilteredAge, lambda1)) +
            scale_fill_scico(palette = 'acton',
                             direction = -1) +
            stat_density_2d(
              aes(fill = after_stat(level), alpha = after_stat(level)),
              bins = 50,
              geom = 'polygon',
              contour_var = 'ndensity',
              h = c(
                provenance::botev(TEdata$FilteredAge),
                provenance::botev(TEdata$lambda1)
              ),
              show.legend = FALSE,
              na.rm = TRUE
            ) +
            geom_point(
              size = 0.75,
              alpha = 0.5,
              na.rm = TRUE,
              aes(shape = Sample)
            ) +
            xlab(NULL) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression(lambda[1])) +
            guides(col = guide_legend(ncol = 1))
        }
        pl2vAge <- {
          ggplot(TEdata,
                 aes(FilteredAge, lambda2)) +
            scale_fill_scico(palette = 'acton',
                             direction = -1) +
            stat_density_2d(
              aes(fill = after_stat(level), alpha = after_stat(level)),
              bins = 50,
              geom = 'polygon',
              contour_var = 'ndensity',
              h = c(
                provenance::botev(TEdata$FilteredAge),
                provenance::botev(TEdata$lambda2)
              ),
              show.legend = FALSE,
              na.rm = TRUE
            ) +
            geom_point(
              size = 0.75,
              alpha = 0.5,
              na.rm = TRUE,
              aes(shape = Sample)
            ) +
            xlab(NULL) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression(lambda[2])) +
            guides(col = guide_legend(ncol = 1))
        }
        pl3vAge <- {
          ggplot(TEdata,
                 aes(FilteredAge, lambda3)) +
            scale_fill_scico(palette = 'acton',
                             direction = -1) +
            stat_density_2d(
              aes(fill = after_stat(level), alpha = after_stat(level)),
              bins = 50,
              geom = 'polygon',
              contour_var = 'ndensity',
              h = c(
                provenance::botev(TEdata$FilteredAge),
                provenance::botev(TEdata$lambda3)
              ),
              show.legend = FALSE,
              na.rm = TRUE
            ) +
            geom_point(
              size = 0.75,
              alpha = 0.5,
              na.rm = TRUE,
              aes(shape = Sample)
            ) +
            xlab(NULL) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression(lambda[3]), x = "Age (Ma)") +
            guides(col = guide_legend(ncol = 1))
        }
      }
      #Binned box plots, 50 Ma----
      {
        pYbU_box <- {
          ggplot(TEdata,
                 aes(FilteredAge, YbU, group = cut_width(FilteredAge, 50))) +
            geom_boxplot(
              varwidth = TRUE,
              fill = "#a56c99",
              outlier.alpha = 0.5,
              outlier.size = 0.5,
              na.rm = TRUE
            ) +
            xlab(NULL) +
            scale_y_log10(breaks = scales::breaks_log(),
                          labels = scales::label_number()) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            coord_cartesian(clip = "off") +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.background = element_rect(fill = "#f9f9f9"),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression("" ^ 172 * "Yb/" ^ 238 * "U")) +
            guides(col = guide_legend(ncol = 1))
        }
        pCeAn_box <- {
          ggplot(TEdata,
                 aes(FilteredAge, Ce_MFR, group = cut_width(FilteredAge, 50))) +
            geom_boxplot(
              varwidth = TRUE,
              fill = "#a56c99",
              outlier.alpha = 0.5,
              outlier.size = 0.5,
              na.rm = TRUE
            ) +
            xlab(NULL) +
            scale_y_log10(breaks = scales::breaks_log(),
                          labels = scales::label_number()) +
            annotation_logticks(
              sides = "l",
              short = unit(-0.1, "cm"),
              mid = unit(-0.2, "cm"),
              long = unit(-0.3, "cm")
            ) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.background = element_rect(fill = "#f9f9f9"),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression("Log"[10] * "(Ce" ^ "*" * ")")) +
            guides(col = guide_legend(ncol = 1))
        }
        pEuAn_box <- {
          ggplot(TEdata,
                 aes(FilteredAge, Eu_MFR, group = cut_width(FilteredAge, 50))) +
            geom_boxplot(
              varwidth = TRUE,
              fill = "#a56c99",
              outlier.alpha = 0.5,
              outlier.size = 0.5,
              na.rm = TRUE
            ) +
            xlab(NULL) +
            scale_y_log10(breaks = scales::breaks_log(),
                          labels = scales::label_number()) +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.background = element_rect(fill = "#f9f9f9"),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression("Eu" ^ "*")) +
            guides(col = guide_legend(ncol = 1))
        }
        pl1_box <- {
          ggplot(TEdata,
                 aes(FilteredAge, lambda1, group = cut_width(FilteredAge, 50))) +
            geom_boxplot(
              varwidth = TRUE,
              fill = "#a56c99",
              outlier.alpha = 0.5,
              outlier.size = 0.5,
              na.rm = TRUE
            ) +
            xlab(NULL) +
            scale_y_continuous() +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.background = element_rect(fill = "#f9f9f9"),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression(lambda[1])) +
            guides(col = guide_legend(ncol = 1))
        }
        pl2_box <- {
          ggplot(TEdata,
                 aes(FilteredAge, lambda2, group = cut_width(FilteredAge, 50))) +
            geom_boxplot(
              varwidth = TRUE,
              fill = "#a56c99",
              outlier.alpha = 0.5,
              outlier.size = 0.5,
              na.rm = TRUE
            ) +
            xlab(NULL) +
            scale_y_continuous() +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.background = element_rect(fill = "#f9f9f9"),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression(lambda[2])) +
            guides(col = guide_legend(ncol = 1))
        }
        pl3_box <- {
          ggplot(TEdata,
                 aes(FilteredAge, lambda3, group = cut_width(FilteredAge, 50))) +
            geom_boxplot(
              varwidth = TRUE,
              fill = "#a56c99",
              outlier.alpha = 0.5,
              outlier.size = 0.5,
              na.rm = TRUE
            ) +
            scale_y_continuous() +
            scale_x_continuous(
              limits = c(500, 3500),
              breaks = scales::breaks_extended(n = 6),
              minor_breaks = scales::breaks_extended(n = 31),
              expand = expansion(add = c(50, 0))
            ) +
            theme_classic() +
            theme(
              axis.text.y = element_text(margin = margin(r = 10)),
              panel.background = element_rect(fill = "#f9f9f9"),
              panel.grid.minor.x = element_line(
                size = 0.5,
                linetype = 3,
                colour = "#707070"
              ),
              panel.grid.major.x = element_line(
                size = 0.5,
                linetype = 2,
                colour = "#707070"
              )
            ) +
            labs(y = expression(lambda[3])) +
            guides(col = guide_legend(ncol = 1))
        }
      }
    }
  }
  #combine key zircon geochem plots----
  {
    pZirconGeochem_scatter <- {
      pYbU_scatter + pCeAn_scatter + pEuAn_scatter + pl1vAge + pl2vAge + pl3vAge + plot_layout(ncol = 1, guides = "collect") + plot_annotation(title = "Key Zircon Geochemistry Plots")
    }
    pZirconGeochem_box <- {
      pYbU_box + pCeAn_box + pEuAn_box + pl1_box + pl2_box + pl3_box + plot_layout(ncol = 1, guides = "collect") + plot_annotation(title = "50 Ma Binned Box and Whisker Plots")
    }
  }
  #plot rendering----
  {
    #View plots----
    {
      pZirconGeochem_box
      pZirconGeochem_scatter
      pUYb_Y
      pLanthVio
    }
    #uncomment next line and set working directory if you want to save to another file path----
    # setwd("D:/")
    #save plots as pdf (you can change file names and types here as needed:----
    {
      ggsave("ZirconGeochem_scatter.pdf", plot = pZirconGeochem_scatter)
      ggsave("ZirconGeochem_box.pdf", plot = pZirconGeochem_box)
      ggsave("ContinentalvOceanicZircon.pdf", plot = pUYb_Y)
      ggsave("LanthanoidViolin.pdf", plot = pLanthVio)
    }
  }
}