package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.temporal.ChronoUnit;

import lombok.extern.slf4j.Slf4j;

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
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseScreeningRondeService;
import nl.rivm.screenit.service.BvoAfmeldService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixAfmeldService;
import nl.rivm.screenit.service.colon.ColonAfmeldService;
import nl.rivm.screenit.service.mamma.MammaAfmeldService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Component
public class BaseAfmeldServiceImpl implements BaseAfmeldService
{
	@Autowired
	private ClientService clientService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseScreeningRondeService screeningRondeService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private LogService logService;

	@Autowired(required = false)
	private ColonAfmeldService colonAfmeldService;

	@Autowired(required = false)
	private CervixAfmeldService cervixAfmeldService;

	@Autowired(required = false)
	private MammaAfmeldService mammaAfmeldService;

	@Override
	@Transactional
	public void definitieveAfmeldingAanvragen(Client client, Afmelding<?, ?, ?> afmelding, boolean rappelBrief, Account account)
	{
		LOG.info("Formulier definitieve afmelding {} aanvragen voor client(id: '{}')", afmelding.getBevolkingsonderzoek().getAfkorting(), client.getId());

		var dossier = koppelDefinitieveAfmelding(client, afmelding);
		hibernateService.saveOrUpdate(dossier);

		saveAfmeldingMetMetadata(afmelding);
		eenmaligAfmeldenVoorDefinitieveAfmelding(afmelding, account);
		bvoDefinitieveOfTijdelijkeAfmeldingAanvragen(afmelding, rappelBrief);
	}

	@Override
	@Transactional
	public void tijdelijkeAfmeldingAanvragen(Client client, Afmelding<?, ?, ?> afmelding, boolean rappelBrief, Account account)
	{
		LOG.info("Formulier tijdelijke afmelding {} aanvragen voor client(id: '{}')", afmelding.getBevolkingsonderzoek().getAfkorting(), client.getId());

		var dossier = koppelTijdelijkeAfmelding(client, afmelding);
		hibernateService.saveOrUpdate(dossier);

		saveAfmeldingMetMetadata(afmelding);
		eenmaligAfmeldenVoorTijdelijkeAfmelding(afmelding, account);
		bvoDefinitieveOfTijdelijkeAfmeldingAanvragen(afmelding, rappelBrief);
	}

	private void saveAfmeldingMetMetadata(Afmelding afmelding)
	{
		afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
		var nu = currentDateSupplier.getLocalDateTime();
		afmelding.setStatusAfmeldDatum(DateUtil.toUtilDate(nu.plus(20, ChronoUnit.MILLIS)));
		afmelding.setAfmeldDatum(DateUtil.toUtilDate(nu.plus(20, ChronoUnit.MILLIS)));

		hibernateService.saveOrUpdate(afmelding);
	}

	private Dossier koppelTijdelijkeAfmelding(Client client, Afmelding afmelding)
	{
		var dossier = clientService.getDossier(client, afmelding.getBevolkingsonderzoek());

		var laatsteRonde = dossier.getLaatsteScreeningRonde();
		if (afmelding.getId() == null || !laatsteRonde.getAfmeldingen().contains(afmelding))
		{
			laatsteRonde.getAfmeldingen().add(afmelding);
		}
		laatsteRonde.setLaatsteAfmelding(afmelding);
		afmelding.setScreeningRonde(laatsteRonde);
		return dossier;
	}

	private Dossier<?, ?> koppelDefinitieveAfmelding(Client client, Afmelding afmelding)
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

	private boolean eenmaligAfmeldenVoorTijdelijkeAfmelding(Afmelding<?, ?, ?> afmelding, Account account)
	{
		var ronde = afmelding.getScreeningRonde();
		if (ronde != null && ronde.getStatus() == ScreeningRondeStatus.LOPEND && Boolean.TRUE.equals(ronde.getAangemeld()))
		{
			var dossier = ronde.getDossier();
			ronde.setAfgerondReden("Formulier definitieve/tijdelijke afmelding aangevraagd");
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			var nu = currentDateSupplier.getLocalDateTime();
			ronde.setStatusDatum(DateUtil.toUtilDate(nu.plus(200, ChronoUnit.MILLIS)));
			ronde.setAangemeld(false);

			clientService.projectClientInactiveren(dossier.getClient(), ProjectInactiefReden.AFMELDING, afmelding.getBevolkingsonderzoek());

			hibernateService.saveOrUpdate(ronde);
			hibernateService.saveOrUpdate(afmelding);

			bvoEenmaligAfmelden(afmelding, account);
			afmelding.setRondeGesloten(true);

			return true;
		}
		return false;
	}

	private boolean eenmaligAfmeldenVoorDefinitieveAfmelding(Afmelding definitieveAfmelding, Account account)
	{
		var ronde = definitieveAfmelding.getDossier().getLaatsteScreeningRonde();

		if (ronde != null && ronde.getStatus() == ScreeningRondeStatus.LOPEND && Boolean.TRUE.equals(ronde.getAangemeld()))
		{
			var afmelding = bvoAfmeldService(definitieveAfmelding).maakAfmelding();
			afmelding.setType(AfmeldingType.EENMALIG);
			afmelding.setImplicieteAfmelding(true);
			afmelding.setScreeningRonde(ronde);

			ronde.getAfmeldingen().add(afmelding);
			ronde.setLaatsteAfmelding(afmelding);
			ronde.setAfgerondReden("Formulier definitieve/tijdelijke afmelding aangevraagd");

			eenmaligAfmelden(afmelding, account);

			hibernateService.saveOrUpdate(afmelding);
			hibernateService.saveOrUpdate(ronde);
			return true;
		}
		return false;
	}

	private void eenmaligAfmelden(Afmelding<?, ?, ?> afmelding, Account account)
	{
		LOG.info("Eenmalig afmelden {} aanvragen voor client(id: '{}')", afmelding.getBevolkingsonderzoek().getAfkorting(),
			afmelding.getScreeningRonde().getDossier().getClient().getId());

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.VERWERKT);
		var nu = currentDateSupplier.getLocalDateTime();
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
	@Transactional
	public void afmelden(Client client, Afmelding<?, ?, ?> afmelding, Account account)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF && afmelding.getAfmeldingStatus() == null)
		{
			definitieveAfmeldingAanvragen(client, afmelding, false, account);
		}
		else if (afmelding.getType() == AfmeldingType.TIJDELIJK && afmelding.getAfmeldingStatus() == null)
		{
			tijdelijkeAfmeldingAanvragen(client, afmelding, false, account);
		}
		else
		{
			var viaInfolijnAangevraagd = !client.equals(account);
			afmeldenZonderVervolg(client, afmelding, viaInfolijnAangevraagd, account);
			bvoVervolgAfmelden(afmelding);
		}
	}

	@Override
	@Transactional
	public void afmeldenZonderVervolg(Client client, Afmelding<?, ?, ?> afmelding, boolean handtekeningDocumentVerplicht, Account account)
	{
		ScreeningRonde ronde;
		Dossier dossier;

		switch (afmelding.getType())
		{
		case EENMALIG:
			ronde = koppelEenmaligeAfmelding(client, afmelding);
			eenmaligAfmelden(afmelding, account);
			ronde.setAfgerondReden("Eenmalige afmelding");
			hibernateService.saveOrUpdate(ronde);
			break;
		case TIJDELIJK:
			if (afmelding.getScreeningRonde() == null && afmelding.getAfmeldingAanvraag() != null)
			{
				zetDefinitieveAfmeldAanvraagOmInTijdelijkeAfmelding((ColonAfmelding) afmelding);
			}
			dossier = koppelTijdelijkeAfmelding(client, afmelding);
			tijdelijkAfmelden(afmelding, handtekeningDocumentVerplicht);
			var eenmaligeAfmeldingVoorTijdelijkeAfmeldingGedaan = eenmaligAfmeldenVoorTijdelijkeAfmelding(afmelding, account);
			hibernateService.saveOrUpdate(dossier);
			if (eenmaligeAfmeldingVoorTijdelijkeAfmeldingGedaan)
			{
				ronde = afmelding.getScreeningRonde();
				ronde.setAfgerondReden("Tijdelijke afmelding");
				hibernateService.saveOrUpdate(ronde);
			}
			break;
		case DEFINITIEF:
			if (afmelding.getDossier() == null && afmelding.getAfmeldingAanvraag() != null)
			{
				zetTijdelijkeAfmeldAanvraagOmInDefinitieveAfmelding((ColonAfmelding) afmelding);
			}
			dossier = koppelDefinitieveAfmelding(client, afmelding);
			definitiefAfmelden(afmelding, handtekeningDocumentVerplicht);
			var eenmaligeAfmeldingGedaan = eenmaligAfmeldenVoorDefinitieveAfmelding(afmelding, account);
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

	private void zetDefinitieveAfmeldAanvraagOmInTijdelijkeAfmelding(ColonAfmelding afmelding)
	{
		var dossier = afmelding.getDossier();
		dossier.setLaatsteAfmelding(null);
		dossier.getAfmeldingen().remove(afmelding);

		var ronde = dossier.getLaatsteScreeningRonde();
		var eenmaligeAfmeldingVoorDefinitieveAfmelding = ronde.getLaatsteAfmelding();

		if (eenmaligeAfmeldingVoorDefinitieveAfmelding != null)
		{
			ronde.getAfmeldingen().remove(eenmaligeAfmeldingVoorDefinitieveAfmelding);
			eenmaligeAfmeldingVoorDefinitieveAfmelding.setScreeningRonde(null);
			hibernateService.delete(eenmaligeAfmeldingVoorDefinitieveAfmelding);
		}

		ronde.setLaatsteAfmelding(afmelding);
		ronde.getAfmeldingen().add(afmelding);

		afmelding.setDossier(null);
		afmelding.setScreeningRonde(ronde);
		afmelding.setRondeGesloten(true);

		updateVolgendeUitnodigingDatumNaTijdelijkAfmelden(afmelding);
	}

	private void zetTijdelijkeAfmeldAanvraagOmInDefinitieveAfmelding(ColonAfmelding afmelding)
	{
		var ronde = afmelding.getScreeningRonde();
		ronde.setLaatsteAfmelding(null);
		ronde.getAfmeldingen().remove(afmelding);

		var dossier = ronde.getDossier();
		dossier.setLaatsteAfmelding(afmelding);
		dossier.getAfmeldingen().add(afmelding);

		afmelding.setScreeningRonde(null);
		afmelding.setDossier(dossier);
		afmelding.setRondeGesloten(false);
	}

	private ScreeningRonde koppelEenmaligeAfmelding(Client client, Afmelding afmelding)
	{
		var ronde = clientService.getDossier(client, afmelding.getBevolkingsonderzoek()).getLaatsteScreeningRonde();
		ronde.getAfmeldingen().add(afmelding);
		ronde.setLaatsteAfmelding(afmelding);
		afmelding.setScreeningRonde(ronde);
		return ronde;
	}

	private void definitiefAfmelden(Afmelding<?, ?, ?> afmelding, boolean handtekeningDocumentVerplicht)
	{
		LOG.info("Definitief afmelden " + afmelding.getBevolkingsonderzoek().getAfkorting() + " aanvragen voor client(id: "
			+ afmelding.getDossier().getClient().getId() + ")");

		var dossier = afmelding.getDossier();
		updateDossierEnAfmeldingDirectEnTijdelijkAfmelden(dossier, afmelding, handtekeningDocumentVerplicht);
	}

	private void tijdelijkAfmelden(Afmelding<?, ?, ?> afmelding, boolean handtekeningDocumentVerplicht)
	{
		LOG.info("Tijdelijk afmelden {} aanvragen voor client(id: {})", afmelding.getBevolkingsonderzoek().getAfkorting(),
			afmelding.getScreeningRonde().getDossier().getClient().getId());

		var dossier = afmelding.getScreeningRonde().getDossier();
		afmelding.setRondeGesloten(true);

		updateVolgendeUitnodigingDatumNaTijdelijkAfmelden((ColonAfmelding) afmelding);
		updateDossierEnAfmeldingDirectEnTijdelijkAfmelden(dossier, afmelding, handtekeningDocumentVerplicht);
	}

	private void updateVolgendeUitnodigingDatumNaTijdelijkAfmelden(ColonAfmelding afmelding)
	{
		var ronde = afmelding.getScreeningRonde();
		var dossier = ronde.getDossier();
		var laatstePeildatum = DateUtil.toLocalDate(ronde.getCreatieDatum());
		var ingangsDatumRondeNaTijdelijkAfmelden = laatstePeildatum.withYear(afmelding.getTijdelijkAfmeldenTotJaartal());

		dossier.getVolgendeUitnodiging().setDatumVolgendeRonde(ingangsDatumRondeNaTijdelijkAfmelden);
	}

	private void updateDossierEnAfmeldingDirectEnTijdelijkAfmelden(Dossier<?, ?> dossier, Afmelding<?, ?, ?> afmelding, boolean handtekeningDocumentVerplicht)
	{
		if (AfmeldingType.DEFINITIEF == afmelding.getType())
		{
			dossier.setAangemeld(false);
		}

		var nu = currentDateSupplier.getLocalDateTime();
		dossier.setInactiefVanaf(DateUtil.toUtilDate(nu));
		dossier.setStatus(DossierStatus.INACTIEF);

		var isHeraangemeldeOnafgerondeTijdelijkeAfmelding =
			afmelding.getAfmeldingStatus() == AanvraagBriefStatus.BRIEF
				&& afmelding.getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT;
		if (isHeraangemeldeOnafgerondeTijdelijkeAfmelding)
		{
			maakHeraanmeldingOngedaan(afmelding);
		}
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

	private static void maakHeraanmeldingOngedaan(Afmelding<?, ?, ?> afmelding)
	{
		afmelding.setHeraanmeldStatus(null);
		afmelding.setStatusHeraanmeldDatum(null);
		afmelding.setHeraanmeldDatum(null);
		afmelding.setRondeHeropend(false);
	}

	private void saveHandtekeningDocumentAfmelding(Client client, Afmelding<?, ?, ?> afmelding)
	{
		try
		{
			var handtekeningDocumentAfmelding = afmelding.getHandtekeningDocumentAfmelding();
			if (handtekeningDocumentAfmelding != null)
			{
				var fileStoreLocation = FileStoreLocation.getAfmelding(afmelding.getBevolkingsonderzoek());
				uploadDocumentService.saveOrUpdate(handtekeningDocumentAfmelding, fileStoreLocation, client.getId());
			}
		}
		catch (IOException e)
		{
			LOG.error("handtekening kon niet worden opgeslagen!", e);
		}
	}

	@Override
	@Transactional
	public <A extends Afmelding<?, ?, ?>> void heraanmelden(A herAanTeMeldenAfmelding, Account account)
	{
		if (herAanTeMeldenAfmelding != null)
		{
			heraanmeldenZonderVervolg(herAanTeMeldenAfmelding);
			vervolgHeraanmelden(herAanTeMeldenAfmelding, account);

			logService.logGebeurtenis(LogGebeurtenis.HERAANMELDEN, account, AfmeldingUtil.getClientFromAfmelding(herAanTeMeldenAfmelding),
				"Type: " + herAanTeMeldenAfmelding.getType().name().toLowerCase() + bvoAfmeldService(herAanTeMeldenAfmelding).getAanvullendeHeraanmeldLogMelding(
					herAanTeMeldenAfmelding), herAanTeMeldenAfmelding.getBevolkingsonderzoek());
		}
	}

	private void vervolgHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding, Account account)
	{
		if (AanvraagBriefStatus.VERWERKT.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()))
		{
			bvoVervolgHeraanmelden(herAanTeMeldenAfmelding, account);
		}
		else if (AanvraagBriefStatus.BRIEF.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()) && AfmeldingType.TIJDELIJK.equals(herAanTeMeldenAfmelding.getType()))
		{
			bvoVervolgHeraanmelden(herAanTeMeldenAfmelding, account);
			heraanmeldenNaTijdelijkAfmeldAanvraag(herAanTeMeldenAfmelding);
		}
	}

	@Override
	@Transactional
	public void heraanmeldenZonderVervolg(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			eenmaligHeraanmelden(herAanTeMeldenAfmelding, false);
			break;
		case TIJDELIJK:
			if (AanvraagBriefStatus.VERWERKT.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()))
			{
				tijdelijkHeraanmelden(herAanTeMeldenAfmelding);
			}
			break;
		case DEFINITIEF:
			if (AanvraagBriefStatus.VERWERKT.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()))
			{
				definitiefHeraanmelden(herAanTeMeldenAfmelding);
			}
			break;
		default:
			throw new IllegalStateException();
		}
	}

	private void tijdelijkHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		LOG.info("Heraanmelden tijdelijke afmelding {} voor client (id: '{}')", herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting(),
			herAanTeMeldenAfmelding.getScreeningRonde().getDossier().getClient().getId());

		var dossier = (ColonDossier) herAanTeMeldenAfmelding.getScreeningRonde().getDossier();
		dossier.getVolgendeUitnodiging().setDatumVolgendeRonde(null);

		dossierEnAfmeldingHeraanmelden(herAanTeMeldenAfmelding, dossier);
		herAanTeMeldenAfmelding.setRondeHeropend(true);

		var ronde = bvoGetGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);
		screeningRondeService.heropenScreeningRonde(ronde);

		var openEenmaligeAfmelding = herAanTeMeldenAfmelding.getScreeningRonde().getAfmeldingen().stream().filter(
			afmelding -> AfmeldingType.EENMALIG.equals(afmelding.getType()) && afmelding.getRondeGesloten() && !afmelding.getRondeHeropend()
		).findFirst();

		openEenmaligeAfmelding.ifPresent(eenmaligeAfmelding -> eenmaligHeraanmelden(eenmaligeAfmelding, true));
	}

	private void heraanmeldenNaTijdelijkAfmeldAanvraag(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		LOG.info("Heraanmelden tijdelijke afmelding {} voor client (id: '{}')", herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting(),
			herAanTeMeldenAfmelding.getScreeningRonde().getDossier().getClient().getId());

		var ronde = bvoGetGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);
		screeningRondeService.heropenScreeningRonde(ronde);
		herAanTeMeldenAfmelding.setRondeHeropend(true);
		herAanTeMeldenAfmelding.setHeraanmeldDatum(currentDateSupplier.getDate());
		herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
		herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(currentDateSupplier.getDate());

		hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
	}

	private void dossierEnAfmeldingHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding, Dossier<?, ?> dossier)
	{
		herAanTeMeldenAfmelding.setHeraanmeldDatum(currentDateSupplier.getDate());
		herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
		herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(currentDateSupplier.getDate());

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

	private void definitiefHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		LOG.info("Heraanmelden definitieve afmelding {} voor client (id: '{}')", herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting(),
			herAanTeMeldenAfmelding.getDossier().getClient().getId());

		dossierEnAfmeldingHeraanmelden(herAanTeMeldenAfmelding, herAanTeMeldenAfmelding.getDossier());
		rondeHeropenenNaDefinitieveAfmelding(herAanTeMeldenAfmelding);
	}

	private void rondeHeropenenNaDefinitieveAfmelding(Afmelding<?, ?, ?> herAanTeMeldenAfmelding)
	{
		var ronde = bvoGetGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);
		if (ronde != null && !ronde.getAangemeld())
		{
			if (ronde.getLaatsteAfmelding() != null)
			{
				eenmaligHeraanmelden(ronde.getLaatsteAfmelding(), true);
			}
			else
			{

				LOG.info("Heraanmelden eenmalige afmelding {} voor client (id: '{}')", herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting(),
					ronde.getDossier().getClient().getId());

				screeningRondeService.heropenScreeningRonde(ronde);
			}
		}
	}

	private void eenmaligHeraanmelden(Afmelding<?, ?, ?> herAanTeMeldenAfmelding, boolean implicieteHeraanmelding)
	{
		LOG.info("Heraanmelden eenmalige afmelding {} voor client (id: '{}')", herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting(),
			herAanTeMeldenAfmelding.getScreeningRonde().getDossier().getClient().getId());

		screeningRondeService.heropenScreeningRonde(herAanTeMeldenAfmelding.getScreeningRonde());

		if (herAanTeMeldenAfmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT)
		{
			var nu = currentDateSupplier.getLocalDateTime();
			herAanTeMeldenAfmelding.setRondeHeropend(true);
			herAanTeMeldenAfmelding.setHeraanmeldDatum(DateUtil.toUtilDate(nu));
			herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
			herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(DateUtil.toUtilDate(nu));
			herAanTeMeldenAfmelding.setImplicieteHeraanmelding(implicieteHeraanmelding);

			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}
	}

	@Override
	@Transactional
	public boolean vervangAfmeldingDocument(UploadDocument nieuwDocument, Afmelding<?, ?, ?> afmelding, UploadDocument huidigDocument, ClientBrief<?, ?, ?> brief,
		Account loggedInAccount)
	{
		afmelding.setHandtekeningDocumentAfmelding(null);
		uploadDocumentService.delete(huidigDocument);

		afmelding.setHandtekeningDocumentAfmelding(nieuwDocument);
		try
		{
			uploadDocumentService.saveOrUpdate(nieuwDocument, FileStoreLocation.getAfmelding(brief.getBevolkingsonderzoek()),
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
	@Transactional
	public boolean vervangHeraanmeldingDocument(UploadDocument nieuwDocument, Afmelding<?, ?, ?> afmelding, UploadDocument huidigDocument, ClientBrief<?, ?, ?> brief,
		Account loggedInAccount)
	{
		afmelding.setHandtekeningDocumentHeraanmelding(null);
		uploadDocumentService.delete(huidigDocument);

		afmelding.setHandtekeningDocumentHeraanmelding(nieuwDocument);
		try
		{
			uploadDocumentService.saveOrUpdate(nieuwDocument, FileStoreLocation.getHeraanmelding(brief.getBevolkingsonderzoek()),
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
	@Transactional
	public void heraanmeldenAlsClientAfgemeldIs(Dossier dossier)
	{
		if (AfmeldingUtil.isEenmaligOfDefinitefAfgemeld(dossier))
		{
			var afmelding = AfmeldingUtil.getLaatsteAfmelding(dossier.getLaatsteScreeningRonde(), dossier);
			heraanmeldenZonderVervolg(afmelding);
		}
	}

	private <A extends Afmelding<?, ?, ?>> void bvoDefinitieveOfTijdelijkeAfmeldingAanvragen(A afmelding, boolean rappelBrief)
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
