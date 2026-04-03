source("R/db_init.R")
out_dir <- "data/parquet"

if(!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
}

tables <- DBI::dbListTables(con)
message("Tables being exported:")
print(tables)

for (tbl in tables) {
    out_file <- file.path(out_dir,paste0(tbl, ".parquet"))

    sql <- paste0("COPY ", DBI::dbQuoteIdentifier(con,tbl), " To '", out_file, "' (FORMAT PARQUET, COMPRESSION ZSTD);")

    message("Exporting ", tbl, " to ", out_file)
    DBI::dbExecute(con,sql)
}

message("Parquet Export done")