# BCFMS-project-management

## Welcome to the Mass-Spectrometry Facility at MPI for Biochemistry

This is the repository of the shiny app for a project management system for the BCF MS core facility

The MS Core Facility provides services such as ​
- Identification of proteins (in-gel or in-solution)
  - “Protein ID”
  - “Protein Coverage”
- In-depth proteome analysis (with and without fractionation)
​  - “Total Proteome”
- Interaction proteomics by Affinity Purification Mass Spectrometry (AP-MS)	
​​​  - “Interaction proteomics”
- Post-translational modification identification (e.g. phosphorylation, ubiquitination, acetylation or glycosylation). Low stoichiometry modifications can be brought into view by enrichment (e.g. Fe-IMAC for phosphorylation)
  - “PTMomics”
  - “Protein Coverage”
- Crosslinking-MS (XL-MS) to extract protein structure distance information
  - “Crosslinking MS”
- Intact mass of proteins and other biomolecules as well as small molecules 
  - “Intact mass”
- Lipidomics / Metabolomics.

We highly recommend to contact us before the start of your experiment. In collaboration with the Bioinformatics Core Facility, we provide:

* Assistance with experimental design of the studies (required number of samples and replicates)
* Assistance with mass spectrometry data interpretation and downstream analysis planning
* Data analysis on collaborative basis

If you have any further questions, please do not hesitate to contact us @ [omicsdesk](mailto:omicsdesk@biochem.mpg.de)!

## Database backups and exact rebuilds

The database creation script preserves an existing database by default. It no longer deletes data unless `MS_RESET_DB=1` is explicitly set.

Create a transactionally consistent, integrity-checked backup:

```bash
Rscript scripts/backup_database.R /srv/shiny-server/ms-app/ms_projects.db /secure/backups/ms_projects.sqlite
```

Restore that exact database on a new machine, then apply any newer schema migrations and seed defaults:

```bash
cd ms-app
MS_DB_FILE=ms_projects.db \
MS_RESTORE_DB_FROM=/secure/backups/ms_projects.sqlite \
MS_RESET_DB=1 \
Rscript setup_database.R
```

The SQLite backup preserves all database rows, including projects, sample records, dropdown options, custom costs, users, and status history. Project uploads are stored outside SQLite and must be backed up separately from the configured `MS_UPLOAD_ROOT` and `MS_LOCAL_UPLOAD_FALLBACK` directories.

For production recovery, keep the database backup and upload-directory backup from the same maintenance window. Test recovery on a separate path before replacing the live database.
