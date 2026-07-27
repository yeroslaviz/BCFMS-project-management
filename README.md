# BCFMS Project Management

Shiny application for managing proteomics, metabolomics, and intact/native mass-spectrometry projects at the MPI for Biochemistry MS Core Facility.

## Documentation

The complete setup and maintenance manual starts at [Documentation](https://yeroslaviz.github.io/BCFMS-project-management/).

- [Quick Start](https://yeroslaviz.github.io/BCFMS-project-management/chapters/quick-start.html)
- [Setup and Infrastructure](https://yeroslaviz.github.io/BCFMS-project-management/chapters/setup-infrastructure.html)
- [LDAP Authentication](https://yeroslaviz.github.io/BCFMS-project-management/chapters/ldap-authentication.html)
- [Troubleshooting](https://yeroslaviz.github.io/BCFMS-project-management/chapters/troubleshooting.html)
- [Deploy and Operations](https://yeroslaviz.github.io/BCFMS-project-management/chapters/deploy-operations.html)
- [Database backup and exact recovery](https://yeroslaviz.github.io/BCFMS-project-management/chapters/deploy-operations.html#database-backup-and-exact-recovery)
- [Change Log](https://yeroslaviz.github.io/BCFMS-project-management/chapters/change-log.html)

## Local start

From the repository root:

```r
Sys.setenv(AUTH_MODE = "local")
shiny::runApp("ms-app")
```

See the [Quick Start](chapters/quick-start.qmd) for database initialization, test-login details, and local LDAP simulation.

## Production

Use the documented [deployment procedure](chapters/deploy-operations.qmd#routine-deploy). A routine deployment preserves the production database and runtime configuration.

Before database maintenance or migration to another machine, follow the [verified backup and exact recovery procedure](chapters/deploy-operations.qmd#database-backup-and-exact-recovery). Uploaded project files must be backed up separately from the SQLite database.

## Contact

Questions about the repository or facility workflows can be sent to [omicsdesk](mailto:omicsdesk@biochem.mpg.de).


quick-start.html
