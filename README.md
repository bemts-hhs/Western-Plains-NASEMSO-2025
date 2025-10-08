# Western Plains NASEMSO 2025 Presentation  

This repository contains the Quarto source code and supporting materials for a presentation at the **Western Plains NASEMSO 2025**. The presentation is delivered entirely as an interactive HTML slide deck using the [Quarto](https://quarto.org/) framework with [Reveal.js](https://revealjs.com/) as the underlying engine.  

## Project Overview  

The purpose of this project is to demonstrate advanced data science methods and applications in trauma and emergency medical services research. This presentation integrates epidemiological principles, applied statistics, and reproducible analytics workflows into a single, cohesive communication format.  

By using Quarto, we:  

- Ensure full reproducibility of analytic outputs.  
- Integrate R-based statistical computing and visualization directly into presentation slides.  
- Apply a consistent and professional institutional theme for visual identity.  
- Disseminate findings and methods in a web-native, portable format suitable for both live presentation and asynchronous review.  

## Technical Details  

- **Framework**: Quarto
- **Presentation Engine**: Reveal.js  
- **Theme**: Custom extension of [letterbox-revealjs](https://github.com/EmilHvitfeldt/quarto-revealjs-letterbox) with institutional colors and fonts.  
- **Primary Language**: R  
- **Font**: [Work Sans](https://fonts.google.com/specimen/Work+Sans) (imported via Google Fonts)  

The presentation leverages a custom SCSS file (`custom.scss`) to enforce typography and color consistency with organizational branding standards. This ensures clarity, accessibility, and visual coherence.  

## Usage  

To render the slides locally:  

```bash
quarto preview slides.qmd
```

# Structure

- `slides.qmd` – Main Quarto document containing content and structure of the presentation.
- `custom.scss` – Custom styling rules that extend the letterbox Reveal.js theme.

# Customization

This repository includes a tailored SCSS file (`custom.scss`) that defines institutional colors, typography, and slide-level styling. Users can adapt this file to create their own branded Quarto Reveal.js themes by modifying:

- Colors: Defined in the scss:defaults section for primary, secondary, and accent palettes.
- Fonts: Swap out the Work Sans import with a preferred Google Font or a local typeface.
- Title Slide Attributes: Set in the YAML front matter using title-slide-attributes (e.g., data-background-color, class, etc.).
- Logos: Adjust positioning and scaling via .slide-logo rules in SCSS.

# Acknowledgments

This project builds upon the open-source contributions of the Quarto and Reveal.js communities, with particular thanks to [Emil Hvitfeldt](https://github.com/EmilHvitfeldt/) for the letterbox-revealjs theme, which provided the structural foundation for the visual design.

# Citation

If referencing this presentation or related methods, please cite as:

State of Iowa Department of Health and Human Services. Bureau of Emergency
Medical and Trauma Services. Western Plains NASEMSO Meeting 2025. Des Moines:
Division of Public Health, 2025. Web.
https://bemts-hhs.github.io/Western-Plains-NASEMSO-2025/. Access date – day month
year.