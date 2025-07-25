% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recruit_model.R
\name{empirical_ssb}
\alias{empirical_ssb}
\title{Empirical Recruits Per Spawning Biomass Distribution (SSB) (Model #2)}
\description{
Empirical Recruits Per Spawning Biomass Distribution (SSB) (Model #2)

Empirical Recruits Per Spawning Biomass Distribution (SSB) (Model #2)
}
\details{
The empirical recruits per spawning biomass distribution model depends on
spawning biomass and is time-invariant.
}
\section{Super classes}{
\code{\link[ageproR:recruit_model]{ageproR::recruit_model}} -> \code{\link[ageproR:empirical_recruit]{ageproR::empirical_recruit}} -> \code{empirical_ssb}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-empirical_ssb-new}{\code{empirical_ssb$new()}}
\item \href{#method-empirical_ssb-clone}{\code{empirical_ssb$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="ageproR" data-topic="empirical_recruit" data-id="inp_lines_recruit_data"><a href='../../ageproR/html/empirical_recruit.html#method-empirical_recruit-inp_lines_recruit_data'><code>ageproR::empirical_recruit$inp_lines_recruit_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ageproR" data-topic="empirical_recruit" data-id="new_obs_table"><a href='../../ageproR/html/empirical_recruit.html#method-empirical_recruit-new_obs_table'><code>ageproR::empirical_recruit$new_obs_table()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ageproR" data-topic="empirical_recruit" data-id="print"><a href='../../ageproR/html/empirical_recruit.html#method-empirical_recruit-print'><code>ageproR::empirical_recruit$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ageproR" data-topic="empirical_recruit" data-id="print_json"><a href='../../ageproR/html/empirical_recruit.html#method-empirical_recruit-print_json'><code>ageproR::empirical_recruit$print_json()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ageproR" data-topic="empirical_recruit" data-id="read_inp_lines"><a href='../../ageproR/html/empirical_recruit.html#method-empirical_recruit-read_inp_lines'><code>ageproR::empirical_recruit$read_inp_lines()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ageproR" data-topic="empirical_recruit" data-id="set_obs_table_from_df"><a href='../../ageproR/html/empirical_recruit.html#method-empirical_recruit-set_obs_table_from_df'><code>ageproR::empirical_recruit$set_obs_table_from_df()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-empirical_ssb-new"></a>}}
\if{latex}{\out{\hypertarget{method-empirical_ssb-new}{}}}
\subsection{Method \code{new()}}{
Initializes the Empirical Recruits Per Spawning Biomass
Distribution Model
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{empirical_ssb$new(num_observations, obs_table = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num_observations}}{\cr
Number of Empirical Observation Records}

\item{\code{obs_table}}{Data Frame containing empirical recruitment
observation table. If NULL, set to default values.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-empirical_ssb-clone"></a>}}
\if{latex}{\out{\hypertarget{method-empirical_ssb-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{empirical_ssb$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
