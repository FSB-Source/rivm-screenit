package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.IntervalcarcinoomProcessdataVerwerkingService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

public class KoppelresultatenKankerregistratieVerwerkenThread extends OpenHibernate5SessionInThread
{

	private static final Logger LOG = LoggerFactory.getLogger(KoppelresultatenKankerregistratieVerwerkenThread.class);

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private IntervalcarcinoomProcessdataVerwerkingService intervalcarcinoomProcessdataVerwerkingService;

	@Autowired
	private FileService fileService;

	private File file;

	private Bevolkingsonderzoek bevolkingsonderzoek;

	private Long igId;

	private String contentType;

	private String fileName;

	public KoppelresultatenKankerregistratieVerwerkenThread(File file, String contentType, String fileName, InstellingGebruiker ingelogdeGebruiker,
		Bevolkingsonderzoek bevolkingsonderzoek)
	{
		this.file = file;
		this.contentType = contentType;
		this.fileName = fileName;
		this.bevolkingsonderzoek = bevolkingsonderzoek;
		this.igId = ingelogdeGebruiker.getId();
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	protected void runInternal()
	{

		List<String> meldingen = new ArrayList<>();
		try (KoppelresultatenKankerregistratieVerwerkingContext context = new KoppelresultatenKankerregistratieVerwerkingContext(file, contentType, fileName, bevolkingsonderzoek))
		{
			fileService.saveOrUpdateUploadDocument(context.getFile(), FileStoreLocation.COLON_INTERVALCARCINOOM);

			while (context.isErEenNieuweRegel())
			{

				intervalcarcinoomProcessdataVerwerkingService.verwerkRegel(context);
			}
			meldingen.add("Aantal regels verwerkt: " + (context.getRegelnummer() - 1));
			meldingen.addAll(context.getMeldingen());
		}
		catch (IllegalStateException e)
		{
			meldingen.add("Er is een fout opgetreden " + e.getMessage());
			LOG.error("Er is een fout opgetreden", e);
		}
		catch (Exception e)
		{
			meldingen.add("Er is een onbekende fout opgetreden " + e.getMessage());
			LOG.error("Er is een onbekende fout opgetreden.", e);
		}
		String melding = StringUtils.join(meldingen, "<br>");
		logService.logGebeurtenis(LogGebeurtenis.UPLOAD_KOPPELRESULTATEN_KANKERREGISTRATIE, hibernateService.get(InstellingGebruiker.class, igId), melding, bevolkingsonderzoek);
	}

}
