package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.File;
import java.io.IOException;

import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectBestandVerwerkingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class ProjectBestandVerwerkThread extends OpenHibernate5SessionInThread
{

	private static final Logger LOG = LoggerFactory.getLogger(ProjectBestandVerwerkThread.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ProjectBestandVerwerkingService projectBestandVerwerkingService;

	@Autowired
	private FileService fileService;

	private Long id;

	public ProjectBestandVerwerkThread(Long id)
	{
		this.id = id;
	}

	@Override
	protected void runInternal()
	{

		ProjectBestand bestand = hibernateService.load(ProjectBestand.class, id);
		ProjectBestandVerwerkingContext context = null;

		ProjectBestandVerwerking verwerking = new ProjectBestandVerwerking();
		verwerking.setProjectBestand(bestand);
		bestand.setVerwerking(verwerking);

		try
		{
			File file = fileService.load(bestand.getUploadDocument());
			context = new ProjectBestandVerwerkingContext(bestand, file);
			projectBestandVerwerkingService.voorbereidingVoorVerwerking(context, bestand);

			while (context.isErEenNieuweRegel())
			{

				projectBestandVerwerkingService.verwerkRegel(context);
			}
			projectBestandVerwerkingService.setBestandStatus(bestand, BestandStatus.VERWERKT);
			if (bestand.getVerwerking().getRegelsMislukt() == 0)
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_BESTAND_VERWERKT, null, getLoggingMelding(bestand));
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_BESTAND_VERWERKT_MET_FOUTEN, null, getLoggingMelding(bestand));
			}
		}
		catch (IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden", e);
			projectBestandVerwerkingService.setBestandStatus(bestand, BestandStatus.CRASH, e.getMessage());
		}
		catch (Exception e)
		{
			LOG.error("Er is een onbekende fout opgetreden.", e);
			projectBestandVerwerkingService.setBestandStatus(bestand, BestandStatus.CRASH, "Er is een fout opgetreden, neem contact op met de helpdesk.");
		}
		finally
		{

			if (context != null)
			{
				try
				{
					context.close();
				}
				catch (IOException e)
				{
					LOG.error("Er is een fout opgetreden bij het sluiten van de reader.", e);
					projectBestandVerwerkingService.setBestandStatus(bestand, BestandStatus.CRASH, "Er is een onbekende fout opgetreden, neem contact op met de helpdesk.");
				}
			}
		}
	}

	private String getLoggingMelding(ProjectBestand bestand)
	{
		String melding = "";
		if (ProjectBestandType.POPULATIE.equals(bestand.getType()))
		{
			melding += bestand.getVerwerking().getRegelsVerwerkt() + " cli&euml;nt(en) toegevoegd voor ";
		}
		else if (ProjectBestandType.INACTIVEREN.equals(bestand.getType()))
		{
			melding += bestand.getVerwerking().getRegelsVerwerkt() + " cli&euml;nt(en) ge&iuml;nactiveerd voor ";
		}
		else if (ProjectBestandType.HERACTIVEREN.equals(bestand.getType()))
		{
			melding += bestand.getVerwerking().getRegelsVerwerkt() + " cli&euml;nt(en) geheractiveerd voor ";
		}
		else if (ProjectBestandType.VERWIJDEREN.equals(bestand.getType()))
		{
			melding += bestand.getVerwerking().getRegelsVerwerkt() + " cli&euml;nt(en) verwijderd voor ";
		}
		else if (ProjectBestandType.ATTRIBUTEN.equals(bestand.getType()))
		{
			melding += "Attributen voor " + bestand.getVerwerking().getRegelsVerwerkt() + " cli&euml;nt(en) verwerkt voor ";
		}

		melding += "project: " + bestand.getProject().getNaam();

		if (bestand.getGroep() != null)
		{
			melding += " (groep: " + bestand.getGroep().getNaam() + ")";
		}

		melding += ", door bestand: " + bestand.getUploadDocument().getNaam();

		if (bestand.getVerwerking().getRegelsMislukt() > 0)
		{
			melding += " (" + bestand.getVerwerking().getRegelsMislukt() + " regels mislukt)";
		}
		return melding;
	}
}
