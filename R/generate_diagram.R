library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

diagram <- grViz("
  digraph functional_pipeline {
    # 1. Global Settings
    graph [layout = dot, 
           rankdir = TB, 
           ratio = 1.0,
           size = '7,7!',
           splines = ortho, 
           nodesep = 0.2, 
           ranksep = 0.2,
           fontname = 'Helvetica',
           fontsize = 12]
    
    # Global node settings
    node [shape = rectangle, 
          style = 'filled, rounded', 
          fontname = 'Helvetica', 
          fontsize = 11,
          color = '#4A4A4A',
          margin = 0.2]
          
    # Global edge settings
    edge [fontname = 'Helvetica', 
          fontsize = 10,
          color = '#333333']

    # --- CLUSTER 1: INPUTS ---
    subgraph cluster_inputs {
      labeljust = 'l';
      label = <<b>1. System Inputs</b>>;
      color = '#e0e0e0'; 
      penwidth = 0.5;
      fontname = 'Helvetica';
      { rank = same; registry; }
      config [label = 'YAML Config Template\\n(Mappings & Types)', fillcolor = '#D1E8FF']
      registry [label = 'Central Registry\\n(Google Sheets/CSV)', fillcolor = '#D1E8FF']
    }

    # --- CLUSTER 2: PROCESSING ---
    subgraph cluster_logic {
      labeljust = 'l';
      label = <<b>2. Ingestion &amp; Logic</b>>; # Escaped &
      color = '#e0e0e0'; 
      penwidth = 0.5;
      fontname = 'Helvetica';
      ingest [label = 'Generalized Ingest\\n(Date/Null Parsing)', fillcolor = '#D1FFD1']
      { rank = same; pivot; taxon; }
      pivot [label = 'Dynamic Column Pivot\\n(Wide to Long)', fillcolor = '#D1FFD1']
      taxon [label = 'Taxonomic Mapping\\n(Fallback Logic)', fillcolor = '#D1FFD1']
    }

    # --- CLUSTER 3: QUALITY & DB ---
    subgraph cluster_storage {
      labeljust = 'l';
      label = <<b>3. Validation &amp; DB</b>>; # Escaped &
      color = '#e0e0e0'; 
      margin = 15;
      penwidth = 0.5;
      fontname = 'Helvetica';
      { rank = same; validate; duckdb; }
      validate [label = 'Data Validation\\n(Config Constraints)', fillcolor = '#FFF2CC']
      duckdb [label = 'DuckDB Instance', fillcolor = '#FFF2CC']
    }

    # --- CLUSTER 4: CLOUD & OPS ---
    subgraph cluster_cloud {
      labeljust = 'l';
      label = <<b>4. Distribution &amp; Ops</b>>; # Escaped &
      color = '#e0e0e0'; 
      penwidth = 0.5;
      fontname = 'Helvetica';
      { rank = same; parquet; gcs; actions; }
      parquet [label = 'Parquet Export\\n(Versioning)', fillcolor = '#F8CECC']
      gcs [label = 'GCS Data Lake\\n(Cloud Storage)', fillcolor = '#F8CECC']
      actions [label = 'GitHub Actions\\n(Weekly Sync)', fillcolor = '#F8CECC']
    }

    # --- BRANCHED FLOW ---
    registry -> ingest
    ingest -> pivot
    pivot -> taxon
    taxon -> validate
    validate -> duckdb
    duckdb -> parquet
    
    actions -> config [label = 'Iterates', style = dashed]
    gcs -> actions [style = dotted, dir = back]
  }
")

svg_content <- export_svg(diagram)
img_path <- file.path(dirname(getwd()), "img")
dir.create(img_path, showWarnings = TRUE, recursive = TRUE)

# 3. Check SVG content was captured
nchar(svg_content)  # should be a large number, not 0

# 4. Save with full absolute path
out_file <- file.path(img_path, "pipeline_diagram.png")
rsvg_png(tmp, out_file, width = 1400)

# 5. Confirm file exists
file.exists(out_file)