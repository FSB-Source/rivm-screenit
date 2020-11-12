package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class ProjectUitslagVerwerkThread extends OpenHibernate5SessionInThread
{
	private static final Logger LOG = LoggerFactory.getLogger(ProjectUitslagVerwerkThread.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ProjectUitslagVerwerkingService projectUitslagVerwerkingService;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	private final Long id;

	public ProjectUitslagVerwerkThread(Long id)
	{
		this.id = id;
	}

	@Override
	protected void runInternal()
	{
		ProjectBestand uitslagenbestand = hibernateService.load(ProjectBestand.class, id);
		ProjectUitslagVerwerkingContext context = null;

		ProjectBestandVerwerking verwerking = new ProjectBestandVerwerking();
		verwerking.setProjectBestand(uitslagenbestand);
		uitslagenbestand.setVerwerking(verwerking);

		try
		{
			File file = fileService.load(uitslagenbestand.getUploadDocument());
			context = new ProjectUitslagVerwerkingContext(uitslagenbestand, file);

			while (context.isErEenNieuweRegel())
			{
				projectUitslagVerwerkingService.verwerkRegel(context);
			}
			projectUitslagVerwerkingService.setBestandStatus(uitslagenbestand, BestandStatus.VERWERKT);
			if (uitslagenbestand.getVerwerking().getRegelsMislukt() == 0)
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_UITSLAG_VERWERKT, null, getLoggingMelding(uitslagenbestand));
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_UITSLAG_VERWERKT_MET_FOUTEN, null, getLoggingMelding(uitslagenbestand));
			}
		}
		catch (IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden", e);
			projectUitslagVerwerkingService.setBestandStatus(uitslagenbestand, BestandStatus.CRASH, e.getMessage());
		}
		catch (Exception e)
		{
			LOG.error("Er is een onbekende fout opgetreden.", e);
			projectUitslagVerwerkingService.setBestandStatus(uitslagenbestand, BestandStatus.CRASH, "Er is een fout opgetreden, neem contact op met de helpdesk.");
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
					projectUitslagVerwerkingService.setBestandStatus(uitslagenbestand, BestandStatus.CRASH, "Er is een onbekende fout opgetreden, neem contact op met de helpdesk.");
				}
			}
		}
	}

	private String getLoggingMelding(ProjectBestand uitslag)
	{
		String melding = uitslag.getVerwerking().getRegelsVerwerkt() + " uitslagen toegevoegd voor project: " + uitslag.getProject().getNaam() + " door bestand: "
			+ uitslag.getUploadDocument().getNaam();

		if (uitslag.getVerwerking().getRegelsMislukt() > 0)
		{
			melding += " (" + uitslag.getVerwerking().getRegelsMislukt() + " regels mislukt)";
		}
		return melding;
	}
}
