package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.MammaUploadBeeldenVerzoekDto;
import nl.rivm.screenit.main.dao.mamma.MammaUploadBeeldenDao;
import nl.rivm.screenit.main.service.mamma.MammaUploadBeeldenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUploadBeeldenVerzoekType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaUploadBeeldenServiceImpl implements MammaUploadBeeldenService
{
	@Autowired
	private MammaUploadBeeldenDao uploadBeeldenVerzoekDao;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseScreeningrondeService screeningrondeService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Override
	public List<MammaUploadBeeldenVerzoek> zoekOpenstaandeUploadBeeldenVerzoeken(Instelling instelling, ScreeningOrganisatie regio, int first, int count,
		SortState<String> sortState)
	{
		return uploadBeeldenVerzoekDao.zoekOpenstaandeUploadBeeldenVerzoeken(instelling, regio, first, count, sortState);
	}

	@Override
	public long countOpenstaandeUploadBeeldenVerzoeken(Instelling instelling, ScreeningOrganisatie regio)
	{
		return uploadBeeldenVerzoekDao.countOpenstaandeUploadBeeldenVerzoeken(instelling, regio);
	}

	@Override
	public List<MammaUploadBeeldenVerzoekDto> zoekInstellingenMetOpenstaandeUploadVerzoeken(ScreeningOrganisatie regio)
	{
		return uploadBeeldenVerzoekDao.zoekInstellingenMetOpenstaandeUploadVerzoeken(regio);
	}

	@Override
	public void maakUploadVerzoek(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, Client client, InstellingGebruiker gemaaktDoor)
	{
		uploadBeeldenVerzoek.setGemaaktDoor(gemaaktDoor);
		MammaScreeningRonde screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
		uploadBeeldenVerzoek.setScreeningRonde(screeningRonde);
		screeningRonde.getUploadBeeldenVerzoeken().add(uploadBeeldenVerzoek);
		uploadBeeldenVerzoek.setVerzoekType(MammaUploadBeeldenVerzoekType.HANDMATIG);

		Date nu = dateSupplier.getDate();
		uploadBeeldenVerzoek.setCreatieDatum(nu);
		uploadBeeldenVerzoek.setStatus(MammaUploadBeeldenVerzoekStatus.WACHTEN_OP_UPLOAD);
		uploadBeeldenVerzoek.setStatusDatum(nu);
		uploadBeeldenVerzoek.setZiekenhuis((Instelling) HibernateHelper.deproxy(uploadBeeldenVerzoek.getZiekenhuis()));

		hibernateService.saveOrUpdateAll(uploadBeeldenVerzoek, screeningRonde);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_UPLOAD_VERZOEK, gemaaktDoor, uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(), "Uploadverzoek aangemaakt",
			Bevolkingsonderzoek.MAMMA);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String uploadBeelden(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, List<UploadDocument> uploadDocumenten, InstellingGebruiker loggedInInstellingGebruiker)
	{
		if (uploadDocumenten.size() != uploadDocumenten.stream().map(UploadDocument::getNaam).distinct().count())
		{
			return "geen.unieke.bestandsnaam";
		}
		try
		{
			MammaUploadBeeldenPoging uploadBeeldenPoging = maakUploadBeeldenPoging(uploadBeeldenVerzoek);

			List<UploadDocument> teUploadenBeelden = new ArrayList<>();
			for (UploadDocument uploadDocument : uploadDocumenten)
			{
				teUploadenBeelden.add(uploadDocument);
				uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.MAMMA_UPLOAD_BEELDEN);
			}

			uploadBeeldenPoging.setBestanden(teUploadenBeelden);
			hibernateService.saveOrUpdate(uploadBeeldenPoging);
			logUploadVerzoekGebeurtenis(loggedInInstellingGebruiker, uploadBeeldenVerzoek, "Beelden geupload");
			return "";
		}
		catch (Exception e)
		{
			LOG.error("Er is een technische fout opgetreden", e);
			logUploadVerzoekGebeurtenis(loggedInInstellingGebruiker, uploadBeeldenVerzoek, "Er is een technische fout opgetreden bij het uploaden");
			return "fout.opgetreden";
		}
	}

	private MammaUploadBeeldenPoging maakUploadBeeldenPoging(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek)
	{
		Date nu = dateSupplier.getDate();

		uploadBeeldenVerzoek.setStatus(MammaUploadBeeldenVerzoekStatus.BEELDEN_GEUPLOAD);
		uploadBeeldenVerzoek.setStatusDatum(nu);

		MammaUploadBeeldenPoging uploadBeeldenPoging = new MammaUploadBeeldenPoging();
		uploadBeeldenPoging.setUploadBeeldenVerzoek(uploadBeeldenVerzoek);
		uploadBeeldenVerzoek.setLaatsteUploadPoging(uploadBeeldenPoging);
		uploadBeeldenVerzoek.getUploadPogingen().add(uploadBeeldenPoging);
		uploadBeeldenPoging.setIlmStatus(MammaMammografieIlmStatus.NIET_BESCHIKBAAR);
		uploadBeeldenPoging.setIlmStatusDatum(nu);
		uploadBeeldenPoging.setCreatieDatum(nu);

		hibernateService.saveOrUpdateAll(uploadBeeldenPoging, uploadBeeldenVerzoek);
		return uploadBeeldenPoging;
	}

	@Override
	public void setGeenBeeldenBeschikbaar(MammaUploadBeeldenVerzoek verzoek, InstellingGebruiker instellingGebruiker) throws IllegalStateException
	{
		hibernateService.reload(verzoek);
		if (MammaUploadBeeldenVerzoekStatus.BEELDEN_GEUPLOAD == verzoek.getStatus()
			|| MammaUploadBeeldenVerzoekStatus.VERWERKT == verzoek.getStatus())
		{
			throw new IllegalStateException("Uploadverzoek heeft beelden");
		}
		if (MammaUploadBeeldenVerzoekStatus.GEEN_BEELDEN_BESCHIKBAAR == verzoek.getStatus())
		{
			return;
		}
		verzoek.setStatus(MammaUploadBeeldenVerzoekStatus.GEEN_BEELDEN_BESCHIKBAAR);
		verzoek.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(verzoek);
		logUploadVerzoekGebeurtenis(instellingGebruiker, verzoek, "Geen beelden beschikbaar");
	}

	@Override
	public void annuleerVerzoek(MammaUploadBeeldenVerzoek verzoek, InstellingGebruiker instellingGebruiker) throws IllegalStateException
	{
		hibernateService.reload(verzoek);
		if (MammaUploadBeeldenVerzoekStatus.BEELDEN_GEUPLOAD == verzoek.getStatus()
			|| MammaUploadBeeldenVerzoekStatus.VERWERKT == verzoek.getStatus())
		{
			throw new IllegalStateException("Uploadverzoek heeft beelden");
		}
		if (MammaUploadBeeldenVerzoekStatus.GEANNULEERD == verzoek.getStatus())
		{
			return;
		}
		verzoek.setStatus(MammaUploadBeeldenVerzoekStatus.GEANNULEERD);
		verzoek.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(verzoek);
		logUploadVerzoekGebeurtenis(instellingGebruiker, verzoek, "Uploadverzoek geannuleerd");
	}

	private void logUploadVerzoekGebeurtenis(InstellingGebruiker instellingGebruiker, MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, String melding)
	{
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_UPLOAD_VERZOEK, instellingGebruiker, uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(), melding,
			Bevolkingsonderzoek.MAMMA);
	}
}
