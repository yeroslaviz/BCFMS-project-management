# BCFMS Project Management

Shiny application for managing proteomics, metabolomics, and intact/native mass-spectrometry projects at the MPI for Biochemistry MS Core Facility.

## Documentation

The complete setup and maintenance manual starts at <a href="https://yeroslaviz.github.io/BCFMS-project-management/" target="_blank" rel="noopener noreferrer">Documentation</a>.

- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/quick-start.html" target="_blank" rel="noopener noreferrer">Quick Start</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/setup-infrastructure.html" target="_blank" rel="noopener noreferrer">Setup and Infrastructure</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/ldap-authentication.html" target="_blank" rel="noopener noreferrer">LDAP Authentication</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/troubleshooting.html" target="_blank" rel="noopener noreferrer">Troubleshooting</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/deploy-operations.html" target="_blank" rel="noopener noreferrer">Deploy and Operations</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/dropdown-options.html" target="_blank" rel="noopener noreferrer">Dropdown Terms and Costs</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/deploy-operations.html#database-backup-and-exact-recovery" target="_blank" rel="noopener noreferrer">Database backup and exact recovery</a>
- <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/change-log.html" target="_blank" rel="noopener noreferrer">Change Log</a>

## Local start

From the repository root:

```r
Sys.setenv(AUTH_MODE = "local")
shiny::runApp("ms-app")
```

See the <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/quick-start.html" target="_blank" rel="noopener noreferrer">Quick Start</a> for database initialization, test-login details, and local LDAP simulation.

## Production

Use the documented <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/deploy-operations.html#routine-deploy" target="_blank" rel="noopener noreferrer">deployment procedure</a>. A routine deployment preserves the production database and runtime configuration.

Before database maintenance or migration to another machine, follow the <a href="https://yeroslaviz.github.io/BCFMS-project-management/chapters/deploy-operations.html#database-backup-and-exact-recovery" target="_blank" rel="noopener noreferrer">verified backup and exact recovery procedure</a>. Uploaded project files must be backed up separately from the SQLite database.

## Contact

Questions about the repository or facility workflows can be sent to [omicsdesk](mailto:omicsdesk@biochem.mpg.de).
