# elm-bar-chart

A lightweight, scalable bar chart module for Elm with responsive sizing behavior.

`elm-bar-chart` is designed for layouts that need to adapt as container space changes.  
As the container width grows or shrinks, bar sizing recalculates so the chart remains readable and proportional.

## Why this module

- Built in pure Elm and easy to integrate into existing apps
- Scales with container width for responsive UI use cases
- Includes hover interactions and contextual legend behavior
- Focuses on practical defaults for app dashboards and status views

## Current status

This project is **WIP (Work in Progress)**.  
The core chart behavior is functional, and improvements are actively being made around API ergonomics, customization, and documentation.

## Preview

![Scalable bar chart demo](static/Screenshot%202026-04-26%20at%2012.24.03.png)

## Features

- Dynamic bar width calculation based on measured container width
- SVG rendering for clean, crisp output
- Auto-scaling Y-axis with tick generation
- Interactive legend with highlight behavior
- Resize-aware updates

## Local development

### Requirements

- Node.js + npm
- Elm 0.19

### Install

```bash
npm install
```

### Run demo

```bash
npm run start
```

This builds:

- `examples/Demo.elm` -> `dist/elm.js`
- `tailwind.css` -> `dist/tailwind.build.css` (watch mode)

### Production build

```bash
npm run build
```

## Project structure

- `src/Bar/Chart.elm` - chart module implementation
- `examples/Demo.elm` - runnable demo and interaction playground
- `dist/` - generated assets

## Roadmap (WIP)

- Cleaner public API for customization
- Better legend and tooltip UX
- More chart configuration options (spacing, colors, axis formatting)
- Additional example scenarios

## Contributing

Suggestions and contributions are welcome while the project is evolving.  
If you try it in a real app, feedback on scaling behavior and API usability is especially helpful.
