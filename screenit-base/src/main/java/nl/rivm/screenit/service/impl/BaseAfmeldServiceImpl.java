package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseScreeningRondeService;
import nl.rivm.screenit.service.BvoAfmeldService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixAfmeldService;
import nl.rivm.screenit.service.colon.ColonAfmeldService;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.REQUIRED)
public class BaseAfmeldServiceImpl implements BaseAfmeldService
{
	private static final Logger LOG = LoggerFactory.getLogger(BaseAfmeldServiceImpl.class);

	@Autowired
	private ClientService clientService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseScreeningRondeService screeningRondeService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired(required = false)
	private ColonAfmeldService colonAfmeldService;

	@Autowired(required = false)
	private CervixAfmeldService cervixAfmeldService;

	@Autowired(required = false)
	private MammaAfmeldService mammaAfmeldService;

	@Override
	public void definitieveAfmeldingAanvragen(Client client, Afmelding<?, ?, ?> afmelding, boolean rappelBrief, Account account)
	{
		LOG.info("Formulier definitieve afmelding " + afmelding.getBevolkingsonderzoek().getAfkorting() + " aanvragen voor client(id: "
			+ client.getId() + ")");

		var dossier = koppelDefinitieveAfmelding(client, afmelding);

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		afmelding.setStatusAfmeldDatum(DateUtil.toUtilDate(nu.plus(20, ChronoUnit.MILLIS)));
		afmelding.setAfmeldDatum(DateUtil.toUtilDate(nu.plus(20, ChronoUnit.MILLIS)));

		hibernateService.saveOrUpdate(afmelding);
		hibernateService.saveOrUpdate(dossier);

		eenmaligAfmeldenVoorDefinitieveAfmelding(afmelding, account);

		bvoDefinitieveAfmeldingAanvragen(afmelding, rappelBrief);
	}

	private Dossier koppelDefinitieveAfmelding(Client client, Afmelding afmelding)
	{
		var dossier = clientService.getDossier(client, afmelding.getBevolkingsonderzoek());
		if (afmelding.getId() == null || !dossier.getAfmeldingen().contains(afmelding))
		{
			dossier.getAfmeldingen().add(afmelding);
		}
		dossier.setLaatsteAfmelding(afmelding);
		afmelding.setDossier(dossier);
		return dossier;
	}

	private boolean eenmaligAfmeldenVoorDefinitieveAfmelding(Afmelding definitieveAfmelding, Account account)
	{
		var ronde = definitieveAfmelding.getDossier().getLaatsteScreeningRonde();
		if (ronde != null && ronde.getStatus() == ScreeningRondeStatus.LOPEND && ronde.getAangemeld())
		{
			var afmelding = bvoAfmeldService(definitieveAfmelding).maakAfmelding();
			afmelding.setType(AfmeldingType.EENMALIG);
			afmelding.setImplicieteAfmelding(true);
			afmelding.setScreeningRonde(ronde);

			ronde.getAfmeldingen().add(afmelding);
			ronde.setLaatsteAfmelding(afmelding);
			ronde.setAfgerondReden("Formulier definitieve afmelding aangevraagd");

			eenmaligAfmelden(afmelding, account);

			hibernateService.saveOrUpdate(afmelding);
			hibernateService.saveOrUpdate(ronde);
			return true;
		}
		return false;
	}

	private void eenmaligAfmelden(Afmelding<?, ?, ?> afmelding, Account account)
	{
		LOG.info("Eenmalig afmelden " + afmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
			+ afmelding.getScreeningRonde().getDossier().getClient().getId());

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.VERWERKT);
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		afmelding.setAfmeldDatum(DateUtil.toUtilDate(nu));
		afmelding.setStatusAfmeldDatum(DateUtil.toUtilDate(nu));
		afmelding.setRondeGesloten(true);

		var ronde = afmelding.getScreeningRonde();
		ronde.setStatus(ScreeningRondeStatus.AFGEROND);
		ronde.setStatusDatum(DateUtil.toUtilDate(nu.plus(200, ChronoUnit.MILLIS)));
		ronde.setAangemeld(false);

		clientService.projectClientInactiveren(ronde.getDossier().getClient(), ProjectInactiefReden.AFMELDING, afmelding.getBevolkingsonderzoek());

		hibernateService.saveOrUpdate(afmelding);
		hibernateService.saveOrUpdate(ronde);

		bvoEenmaligAfmelden(afmelding, account);
	}

	@Override
	public void afmelden(Client client, Afmelding<?, ?, ?> afmelding, Account account)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF && afmelding.getAfmeldingStatus() == null)
		{
			definitieveAfmeldingAanvragen(client, afmelding, false, account);
		}
		else
		{
			afmeldenZonderVervolg(client, afmelding, true, account);
			bvoVervolgAfmelden(afmelding);
		}
	}

	@Override
	public void afmeldenZonderVervolg(Client client, Afmelding afmelding, boolean handtekeningDocumentVerplicht, Account account)
	{
		ScreeningRonde ronde;

		switch (afmelding.getType())
		{
		case EENMALIG:
			ronde = koppelEenmaligeAfmelding(client, afmelding);
			eenmaligAfmelden(afmelding, account);
			ronde.setAfgerondReden("Eenmalige afmelding");
			hibernateService.saveOrUpdate(ronde);
			break;
		case DEFINITIEF:
			var dossier = koppelDefinitieveAfmelding(client, afmelding);
			definitiefAfmelden(afmelding, handtekeningDocumentVerplicht);
			boolean eenmaligeAfmeldingGedaan = eenmaligAfmeldenVoorDefinitieveAfmelding(afmelding, account);
			hibernateService.saveOrUpdate(dossier);
			if (eenmaligeAfmeldingGedaan)
			{
				ronde = afmelding.getDossier().getLaatsteScreeningRonde();
				ronde.setAfgerondReden("Definitieve afmelding");
				hibernateService.saveOrUpdate(ronde);
			}
			break;
		default:
			throw new IllegalStateException();
		}

		hibernateService.saveOrUpdate(afmelding);
	}

	private ScreeningRonde koppelEenmaligeAfmelding(Client client, Afmelding afmelding)
	{
		ScreeningRonde ronde = clientService.getDossier(client, afmelding.getBevolkingsonderzoek()).getLaatsteScreeningRonde();
		ronde.getAfmeldingen().add(afmelding);
		ronde.setLaatsteAfmelding(afmelding);
		afmelding.setScreeningRonde(ronde);
		return ronde;
	}

	private void definitiefAfmelden(Afmelding<?, ?, ?> afmelding, boolean handtekeningDocumentVerplicht)
	{
		LOG.info("Definitief afmelden " + afmelding.getBevolkingsonderzoek().getAfkorting() + " voor clientId "
			+ afmelding.getDossier().getClient().getId());

		var dossier = afmelding.getDossier();
		dossier.setAangemeld(false);
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		dossier.setInactiefVanaf(DateUtil.toUtilDate(nu));
		dossier.setStatus(DossierStatus.INACTIEF);

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.VERWERKT);
		afmelding.setStatusAfmeldDatum(DateUtil.toUtilDate(nu.plus(20, ChronoUnit.MILLIS)));
		afmelding.setAfmeldDatum(DateUtil.toUtilDate(nu.plus(20, ChronoUnit.MILLIS)));

		if (handtekeningDocumentVerplicht)
		{
			saveHandtekeningDocumentAfmelding(dossier.getClient(), afmelding);
		}

		hibernateService.saveOrUpdate(afmelding);
		hibernateService.saveOrUpdate(dossier);

		clientService.projectClientInactiveren(dossier.getClient(), ProjectInactiefReden.AFMELDING, afmelding.getBevolkingsonderzoek());
	}

	private void saveHandtekeningDocumentAfmelding(Client client, Afmelding<?, ?, ?> afmelding)
	{
		try
		{
			UploadDocument handtekeningDocumentAfmelding = afmelding.getHandtekeningDocumentAfmelding();
			if (handtekeningDocumentAfmelding != null)
			{
				var fileStoreLocation = FileStoreLocation.getAfmelding(afmelding.getBevolkingsonderzoek());
				fileService.saveOrUpdateUploadDocument(handtekeningDocumentAfmelding, fileStoreLocation, client.getId());
			}
		}
		catch (IOException e)
		{
			LOG.error("handtekening kon niet worden opgeslagen!", e);
		}
	}

	@Override
	public <A extends Afmelding<?, ?, ?>> void heraanmelden(A herAanTeMeldenAfmelding, Account account)
	{
		if (herAanTeMeldenAfmelding != null)
		{
			heraanmeldenZonderVervolg(herAanTeMeldenAfmelding);
			vervolgHeraanmelden(herAanTeMeldenAfmelding, account);

			logService.logGebeurtenis(LogGebeurtenis.HERAANMELDEN, account, AfmeldingUtil.getClientFromAfmelding(herAanTeMeldenAfmelding),
				"Type: " + herAanTeMeldenAfmelding.getType().name().toLowerCase() + bvoAfmeldService(herAanTeMeldenAfmelding).getAanvullendeHeraanmeldLogMelding(
					herAanTeMeldenAfmelding),
				herAanTeMeldenAfmelding.getBevolkingsonderzoek());
		}
	}

	private void vervolgHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding, Account account)
	{
		if (AanvraagBriefStatus.VERWERKT.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()))
		{
			bvoVervolgHeraanmelden(herAanTeMeldenAfmelding, account);
		}
	}

	@Override
	public void heraanmeldenZonderVervolg(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			eenmaligHeraanmelden(herAanTeMeldenAfmelding, false);
			break;
		case DEFINITIEF:
			if (AanvraagBriefStatus.VERWERKT.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()))
			{
				definitiefHeraanmelden(herAanTeMeldenAfmelding);

				var ronde = bvoGetGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);
				if (ronde != null && !ronde.getAangemeld())
				{
					if (ronde.getLaatsteAfmelding() != null)
					{
						eenmaligHeraanmelden(ronde.getLaatsteAfmelding(), true);
					}
					else
					{

						LOG.info("Eenmalig heraanmelden " + herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
							+ ronde.getDossier().getClient().getId());

						screeningRondeService.heropenScreeningRonde(ronde);
					}
				}
			}
			break;
		default:
			throw new IllegalStateException();
		}
	}

	private void definitiefHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		LOG.info("Definitief heraanmelden " + herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
			+ herAanTeMeldenAfmelding.getDossier().getClient().getId());

		herAanTeMeldenAfmelding.setHeraanmeldDatum(currentDateSupplier.getDate());
		herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
		herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(currentDateSupplier.getDate());

		var dossier = herAanTeMeldenAfmelding.getDossier();
		if (dossier.getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON && ((ColonDossier) dossier).getInactiveerReden() == null
			|| dossier.getBevolkingsonderzoek() != Bevolkingsonderzoek.COLON)
		{

			dossier.setStatus(DossierStatus.ACTIEF);
			dossier.setInactiefVanaf(null);
			dossier.setInactiefTotMet(null);
		}
		dossier.setAangemeld(true);

		hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		hibernateService.saveOrUpdate(dossier);
	}

	private void eenmaligHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding, boolean implicieteHeraanmelding)
	{
		LOG.info("Eenmalig heraanmelden " + herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
			+ herAanTeMeldenAfmelding.getScreeningRonde().getDossier().getClient().getId());

		screeningRondeService.heropenScreeningRonde(herAanTeMeldenAfmelding.getScreeningRonde());

		if (herAanTeMeldenAfmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT)
		{
			LocalDateTime nu = currentDateSupplier.getLocalDateTime();
			herAanTeMeldenAfmelding.setRondeHeropend(true);
			herAanTeMeldenAfmelding.setHeraanmeldDatum(DateUtil.toUtilDate(nu));
			herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
			herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(DateUtil.toUtilDate(nu));
			herAanTeMeldenAfmelding.setImplicieteHeraanmelding(implicieteHeraanmelding);

			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}
	}

	@Override
	public boolean vervangAfmeldingDocument(UploadDocument nieuwDocument, Afmelding<?, ?, ?> afmelding, UploadDocument huidigDocument, ClientBrief<?, ?, ?> brief,
		Account loggedInAccount)
	{
		afmelding.setHandtekeningDocumentAfmelding(null);
		fileService.delete(huidigDocument, true);

		afmelding.setHandtekeningDocumentAfmelding(nieuwDocument);
		try
		{
			fileService.saveOrUpdateUploadDocument(nieuwDocument, FileStoreLocation.getAfmelding(brief.getBevolkingsonderzoek()),
				brief.getClient().getId());
		}
		catch (IOException e)
		{
			LOG.error("Fout bij uploaden van een afmeldingformulier met handtekening: ", e);
			return false;
		}

		logService.logGebeurtenis(LogGebeurtenis.VERVANGEN_DOCUMENT, loggedInAccount, brief.getClient(),
			brief.getBriefType() + ", is vervangen.", brief.getBriefType().getOnderzoeken());
		return true;

	}

	@Override
	public boolean vervangHeraanmeldingDocument(UploadDocument nieuwDocument, Afmelding<?, ?, ?> afmelding, UploadDocument huidigDocument, ClientBrief<?, ?, ?> brief,
		Account loggedInAccount)
	{
		afmelding.setHandtekeningDocumentHeraanmelding(null);
		fileService.delete(huidigDocument, true);

		afmelding.setHandtekeningDocumentHeraanmelding(nieuwDocument);
		try
		{
			fileService.saveOrUpdateUploadDocument(nieuwDocument, FileStoreLocation.getHeraanmelding(brief.getBevolkingsonderzoek()),
				brief.getClient().getId());
		}
		catch (IOException e)
		{
			LOG.error("Fout bij uploaden van een afmeldingformulier met handtekening: ", e);
			return false;
		}

		logService.logGebeurtenis(LogGebeurtenis.VERVANGEN_DOCUMENT, loggedInAccount, brief.getClient(),
			brief.getBriefType() + ", is vervangen.", brief.getBriefType().getOnderzoeken());
		return true;

	}

	@Override
	public void heraanmeldenAlsClientAfgemeldIs(Dossier dossier)
	{
		if (AfmeldingUtil.isAfgemeld(dossier))
		{
			var afmelding = AfmeldingUtil.getLaatsteAfmelding(dossier.getLaatsteScreeningRonde(), dossier);
			heraanmeldenZonderVervolg(afmelding);
		}
	}

	private <A extends Afmelding<?, ?, ?>> void bvoDefinitieveAfmeldingAanvragen(A afmelding, boolean rappelBrief)
	{
		bvoAfmeldService(afmelding).definitieveAfmeldingAanvragen(afmelding, rappelBrief);
	}

	private <A extends Afmelding<?, ?, ?>> void bvoEenmaligAfmelden(A afmelding, Account account)
	{
		bvoAfmeldService(afmelding).eenmaligAfmelden(afmelding, account);
	}

	private <A extends Afmelding<?, ?, ?>> void bvoVervolgAfmelden(A afmelding)
	{
		bvoAfmeldService(afmelding).vervolgAfmelden(afmelding);
	}

	private <A extends Afmelding<?, ?, ?>> void bvoVervolgHeraanmelden(A herAanTeMeldenAfmelding, Account account)
	{
		bvoAfmeldService(herAanTeMeldenAfmelding).vervolgHeraanmelden(herAanTeMeldenAfmelding, account);
	}

	private <A extends Afmelding<?, ?, ?>> ScreeningRonde<?, ?, ?, ?> bvoGetGeldigeRondeVoorHeraanmelding(A herAanTeMeldenAfmelding)
	{
		return bvoAfmeldService(herAanTeMeldenAfmelding).getGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);
	}

	private <A extends Afmelding<?, ?, ?>> BvoAfmeldService<A> bvoAfmeldService(A afmelding)
	{
		switch (afmelding.getBevolkingsonderzoek())
		{
		case COLON:
			return (BvoAfmeldService<A>) colonAfmeldService;
		case CERVIX:
			return (BvoAfmeldService<A>) cervixAfmeldService;
		case MAMMA:
			return (BvoAfmeldService<A>) mammaAfmeldService;
		default:
			throw new IllegalStateException();
		}
	}
}
