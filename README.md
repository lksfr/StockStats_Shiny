# StockStats Web App

## Introduction

I have always been interested in analyzing stocks on websites such as Yahoo Finance or Marketwatch. As I began taking statistics, I wanted to better understand the statistical properties of these stocks I had been looking at. However, I just was not able to find such in-depth tools on most major finance websites, especially for free. Therefore, after having gained proficiency in R, I decided to make use of my skills and build something that hopefully not only benefits myself but also other curious data enthusiasts.

The idea behind StockStats is to build a web app that enables everybody with Internet access to build dashboards with statistical analyses and visualizations of stocks. These dashboards can then be saved or revisited at a later point for comparison.

## Motivation

While preparing for this project, I looked at many Shiny dashboards. Although Shiny, an R package, is mainly used to build data visualization dashboards, there is much more one can theoretically achieve with it. My main issues with most dashboards I explored were:

 1. Most dashboards allowed for interactive data visualizations, however, the data they were based on was static and did not       adapt/change

2. As a result, there was little incentive to revisit them after having taken a look at the data once.

## Approach

In order to avoid these two pitfalls, I used the quantmod package to dynamically retrieve stock data. In doing so, I was able to enable users to update their dashboards whenever they revisit the web app. To tackle the second issue of lacking incentives for users to return, I also decided not to use the widely applied Shiny Dashboards framework for my UI design and instead challenged myself by implementing semantic.dashboard, an R package released in April of 2018. semantic.dashboard comes with full integration of Semantic UI, a modern front-end development framework, powered by LESS and jQuery, thus providing more options for UI design.

Because the package was relatively new, working with semantic.dashboard proved to be a challenging yet rewarding experience due to the many options it provides. On top, it also enabled me to push the framework provided by Shiny to create a fully-functional website and not only a dashboard.

#### Link to the project: https://lksfr.shinyapps.io/Shiny/
#### Link to the full blog post: https://nycdatascience.com/blog/student-works/stockstats-a-shiny-web-app-for-investors/
