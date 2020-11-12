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

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.RedenOpnieuwAanvragenClientgegevens;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.enums.RedenAfspraakAfzeggen;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OpenUitnodigingService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.joda.time.DateTime;
import org.joda.time.MutableDateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ClientServiceImpl implements ClientService
{
	
	public static final Pattern POSTCODE_NL = Pattern.compile("[1-9][0-9]{3}?[\\s]?[a-zA-Z]{2}");

	private static final Logger LOG = LoggerFactory.getLogger(ClientServiceImpl.class);

	@Autowired
	private ClientDao clientDao;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private OpenUitnodigingService openUitnodigingService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired(required = false)
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired(required = false)
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired(required = false)
	private AfspraakService afspraakService;

	@Autowired(required = false)
	private CervixFactory cervixFactory;

	@Autowired(required = false)
	private MammaBaseUitstelService mammaBaseUitstelService;

	@Autowired(required = false)
	private CervixBaseScreeningrondeService cervixBaseScreeningrondeService;

	@Autowired(required = false)
	private ColonScreeningsrondeService colonScreeningsrondeService;

	@Autowired(required = false)
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Override
	public Client getClientByBsn(String bsn)
	{
		return clientDao.getClientByBsn(bsn);
	}

	@Override
	public Client getClientZonderBezwaar(String bsn)
	{
		return clientDao.getClientZonderBezwaar(bsn);
	}

	@Override
	public boolean heeftDossierMetRondeOfAfmelding(Client client)
	{
		return clientDao.heeftDossierMetRondeOfAfmelding(client);
	}

	@Override
	public Client getClientByBsnFromNg01Bericht(String bsn, String anummer)
	{
		return clientDao.getClientByBsnFromNg01Bericht(bsn, anummer);
	}

	@Override
	public Client getLaatstAfgevoerdeClient(String bsn)
	{
		return clientDao.getLaatstAfgevoerdeClient(bsn);
	}

	@Override
	public String getVoorNg01EenNieuweBsn(String bsn)
	{
		if (bsn == null) 
		{
			return null;
		}

		Client client = getClientByBsnFromNg01Bericht(bsn, null);
		if (client != null && client.getPersoon().getBsn().length() == 12)
		{
			bsn = client.getPersoon().getBsn();
		}

		int ng01Value = 0;
		if (bsn != null && bsn.length() == 12)
		{
			ng01Value = Integer.parseInt(bsn.substring(0, 3));
			bsn = bsn.substring(3, bsn.length());
		}
		ng01Value++;

		return StringUtils.leftPad(ng01Value + "", 3, '0') + bsn;
	}

	@Override
	public List<Client> zoekClienten(Client zoekObject)
	{
		return clientDao.zoekClienten(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateClient(Client client)
	{
		TijdelijkAdres tijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		if (tijdelijkAdres != null)
		{
			if (tijdelijkAdres.getStartDatum() == null && tijdelijkAdres.getEindDatum() == null || StringUtils.isBlank(tijdelijkAdres.getStraat()))
			{
				if (tijdelijkAdres.getId() != null)
				{
					hibernateService.delete(tijdelijkAdres);
				}
				client.getPersoon().setTijdelijkAdres(null);
			}
		}
		clientDao.saveOrUpdateClient(client);
	}

	@Override
	public List<Client> getClientenMetTitel(String titelCode)
	{
		return clientDao.getClientenMetTitel(titelCode);
	}

	@Override
	public Dossier getDossier(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		switch (bevolkingsonderzoek)
		{
		case COLON:
			return client.getColonDossier();
		case CERVIX:
			return client.getCervixDossier();
		case MAMMA:
			return client.getMammaDossier();
		}
		return null;
	}

	private Dossier koppelDefinitieveAfmelding(Client client, Afmelding afmelding)
	{
		Dossier dossier = getDossier(client, afmelding.getBevolkingsonderzoek());
		if (afmelding.getId() == null || !dossier.getAfmeldingen().contains(afmelding))
		{
			dossier.getAfmeldingen().add(afmelding);
		}
		dossier.setLaatsteAfmelding(afmelding);
		afmelding.setDossier(dossier);
		return dossier;
	}

	private ScreeningRonde koppelEenmaligeAfmelding(Client client, Afmelding afmelding)
	{
		ScreeningRonde ronde = getDossier(client, afmelding.getBevolkingsonderzoek()).getLaatsteScreeningRonde();
		ronde.getAfmeldingen().add(afmelding);
		ronde.setLaatsteAfmelding(afmelding);
		afmelding.setScreeningRonde(ronde);
		return ronde;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void definitieveAfmeldingAanvragen(Client client, Afmelding afmelding, boolean rappelBrief, Account account)
	{
		LOG.info("Formulier definitieve afmelding " + afmelding.getBevolkingsonderzoek().getAfkorting() + " aanvragen voor client(id: " + client.getId() + ")");

		Dossier dossier = koppelDefinitieveAfmelding(client, afmelding);

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
		DateTime nu = currentDateSupplier.getDateTime();
		afmelding.setStatusAfmeldDatum(nu.plusMillis(20).toDate());
		afmelding.setAfmeldDatum(nu.plusMillis(20).toDate());

		hibernateService.saveOrUpdate(afmelding);
		hibernateService.saveOrUpdate(dossier);

		eenmaligAfmeldenVoorDefinitieveAfmelding(afmelding, account);

		switch (afmelding.getBevolkingsonderzoek())
		{
		case COLON:
			colonDefinitieveAfmeldingAanvragen((ColonAfmelding) afmelding, rappelBrief);
			break;
		case CERVIX:
			cervixDefinitieveAfmeldingAanvragen((CervixAfmelding) afmelding, rappelBrief);
			break;
		case MAMMA:
			mammaDefinitieveAfmeldingAanvragen((MammaAfmelding) afmelding, rappelBrief);
			break;
		default:
			throw new IllegalStateException();
		}
	}

	private void mammaDefinitieveAfmeldingAanvragen(MammaAfmelding afmelding, boolean rappelBrief)
	{
		BriefType briefType = BriefType.MAMMA_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.MAMMA_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakMammaBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	private void colonDefinitieveAfmeldingAanvragen(ColonAfmelding afmelding, boolean rappelBrief)
	{
		BriefType briefType = BriefType.COLON_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.COLON_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakColonBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	private void cervixDefinitieveAfmeldingAanvragen(CervixAfmelding afmelding, boolean rappelBrief)
	{
		BriefType briefType = BriefType.CERVIX_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.CERVIX_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakCervixBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void afmeldenZonderVervolg(Client client, Afmelding afmelding, boolean handtekeningDocumentVerplicht, Account account)
	{
		Dossier dossier = null;
		ScreeningRonde ronde = null;

		switch (afmelding.getType())
		{
		case EENMALIG:
			ronde = koppelEenmaligeAfmelding(client, afmelding);
			eenmaligAfmelden(afmelding, account);
			ronde.setAfgerondReden("Eenmalige afmelding");
			hibernateService.saveOrUpdate(ronde);
			break;
		case DEFINITIEF:
			dossier = koppelDefinitieveAfmelding(client, afmelding);
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

	private void definitiefAfmelden(Afmelding afmelding, boolean handtekeningDocumentVerplicht)
	{
		LOG.info("Definitief afmelden " + afmelding.getBevolkingsonderzoek().getAfkorting() + " voor clientId " + afmelding.getDossier().getClient().getId());

		Dossier dossier = afmelding.getDossier();
		dossier.setAangemeld(false);
		DateTime nu = currentDateSupplier.getDateTime();
		dossier.setInactiefVanaf(nu.toDate());
		dossier.setStatus(DossierStatus.INACTIEF);

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.VERWERKT);
		afmelding.setStatusAfmeldDatum(nu.plusMillis(20).toDate());
		afmelding.setAfmeldDatum(nu.plusMillis(20).toDate());

		if (handtekeningDocumentVerplicht)
		{
			saveHandtekeningDocumentAfmelding(dossier.getClient(), afmelding);
		}

		hibernateService.saveOrUpdate(afmelding);
		hibernateService.saveOrUpdate(dossier);

		projectClientInactiveren(dossier.getClient(), ProjectInactiefReden.AFMELDING, afmelding.getBevolkingsonderzoek());
	}

	private void vervolgAfmelden(Afmelding afmelding)
	{
		switch (afmelding.getBevolkingsonderzoek())
		{
		case COLON:
			colonVervolgAfmelden((ColonAfmelding) afmelding);
			break;
		case CERVIX:
			cervixVervolgAfmelden((CervixAfmelding) afmelding);
			break;
		case MAMMA:
			mammaVervolgAfmelden((MammaAfmelding) afmelding);
			break;
		default:
			throw new IllegalStateException();
		}

	}

	private void mammaVervolgAfmelden(MammaAfmelding afmelding)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			afmelding.setAfmeldingBevestiging(briefService.maakMammaBrief(afmelding, BriefType.MAMMA_BEVESTIGING_DEFINITIEVE_AFMELDING, currentDateSupplier.getDate()));
			hibernateService.saveOrUpdate(afmelding);
		}
	}

	private void colonVervolgAfmelden(ColonAfmelding afmelding)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			if (!ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(afmelding.getReden()))
			{
				afmelding.setAfmeldingBevestiging(briefService.maakColonBrief(afmelding, BriefType.COLON_AFMELDING_BEVESTIGING, currentDateSupplier.getDate()));
				hibernateService.saveOrUpdate(afmelding);
			}

			ColonScreeningRonde ronde = afmelding.getDossier().getLaatsteScreeningRonde();
			if (ronde != null && ronde.getOpenUitnodiging() != null && ronde.getOpenUitnodiging().getUitslag() == null)
			{
				OpenUitnodiging uitnodiging = ronde.getOpenUitnodiging();
				uitnodiging.setUitslag(OpenUitnodigingUitslag.AFMELDING);
				uitnodiging.setAfmelding(afmelding);
				hibernateService.saveOrUpdate(uitnodiging);
			}
		}
	}

	private void cervixVervolgAfmelden(CervixAfmelding afmelding)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			afmelding.setAfmeldingBevestiging(briefService.maakCervixBrief(afmelding, BriefType.CERVIX_BEVESTIGING_DEFINITIEVE_AFMELDING, currentDateSupplier.getDate()));
			hibernateService.saveOrUpdate(afmelding);
		}
	}

	private boolean eenmaligAfmeldenVoorDefinitieveAfmelding(Afmelding definitieveAfmelding, Account account)
	{
		ScreeningRonde ronde = definitieveAfmelding.getDossier().getLaatsteScreeningRonde();
		if (ronde != null && ronde.getStatus() == ScreeningRondeStatus.LOPEND && ronde.getAangemeld())
		{
			Afmelding afmelding = maakAfmelding(definitieveAfmelding.getBevolkingsonderzoek());
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

	private Afmelding maakAfmelding(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		switch (bevolkingsonderzoek)
		{
		case COLON:
			return new ColonAfmelding();
		case CERVIX:
			return new CervixAfmelding();
		case MAMMA:
			return new MammaAfmelding();
		default:
			throw new IllegalStateException();
		}
	}

	private void eenmaligAfmelden(Afmelding afmelding, Account account)
	{
		LOG.info("Eenmalig afmelden " + afmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
			+ afmelding.getScreeningRonde().getDossier().getClient().getId());

		afmelding.setAfmeldingStatus(AanvraagBriefStatus.VERWERKT);
		DateTime nu = currentDateSupplier.getDateTime();
		afmelding.setAfmeldDatum(nu.toDate());
		afmelding.setStatusAfmeldDatum(nu.toDate());
		afmelding.setRondeGesloten(true);

		ScreeningRonde ronde = afmelding.getScreeningRonde();
		ronde.setStatus(ScreeningRondeStatus.AFGEROND);
		ronde.setStatusDatum(nu.plusMillis(200).toDate());
		ronde.setAangemeld(false);

		projectClientInactiveren(ronde.getDossier().getClient(), ProjectInactiefReden.AFMELDING, afmelding.getBevolkingsonderzoek());

		hibernateService.saveOrUpdate(afmelding);
		hibernateService.saveOrUpdate(ronde);

		switch (afmelding.getBevolkingsonderzoek())
		{
		case COLON:
			colonEenmaligAfmelden((ColonAfmelding) afmelding, account);
			break;
		case CERVIX:
			cervixEenmaligAfmelden((CervixAfmelding) afmelding);
			break;
		case MAMMA:
			mammaEenmaligAfmelden((MammaAfmelding) afmelding, account);
			break;

		}
	}

	private void mammaEenmaligAfmelden(MammaAfmelding afmelding, Account account)
	{
		MammaScreeningRonde ronde = afmelding.getScreeningRonde();
		afspraakEnUitstelAfzeggen(ronde, account);
		nietVerzondenUitnodigingenVerwijderen(ronde);
	}

	private void nietVerzondenUitnodigingenVerwijderen(MammaScreeningRonde ronde)
	{

	}

	private void afspraakEnUitstelAfzeggen(MammaScreeningRonde ronde, Account account)
	{
		Date nu = currentDateSupplier.getDateTime().plusMillis(100).toDate();

		MammaUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		MammaAfspraak laatsteAfspraak = laatsteUitnodiging != null ? ronde.getLaatsteUitnodiging().getLaatsteAfspraak() : null;

		if (laatsteAfspraak != null)
		{
			MammaAfspraakStatus status = laatsteAfspraak.getStatus();
			status = account instanceof Client ? MammaAfspraakStatus.GEANNULEERD_CLIENT : MammaAfspraakStatus.GEANNULEERD_VIA_INFOLIJN;
			baseAfspraakService.afspraakAnnuleren(laatsteAfspraak, status, nu);
			LOG.info("Afmelding: laatste afspraak is afgezegd");
		}

		if (ronde.getLaatsteUitstel() != null)
		{
			mammaBaseUitstelService.uitstelAfzeggen(ronde.getLaatsteUitstel(), MammaUitstelGeannuleerdReden.AFMELDING, nu);
		}
	}

	private void colonEenmaligAfmelden(ColonAfmelding afmelding, Account account)
	{
		ColonScreeningRonde ronde = afmelding.getScreeningRonde();

		afspraakAfzeggen(ronde);
		nietVerzondenUitnodigingenVerwijderen(ronde);
		openUitnodigingService.afmeldingHeraanmeldingReactieOpOpenUitnodiging(afmelding, ronde, account);
	}

	private void afspraakAfzeggen(ColonScreeningRonde ronde)
	{
		ColonIntakeAfspraak laatsteAfspraak = ronde.getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			ColonConclusieType colonConclusieType = null;
			ColonConclusie conclusie = laatsteAfspraak.getConclusie();
			if (conclusie != null)
			{
				colonConclusieType = conclusie.getType();
			}
			AfspraakStatus status = laatsteAfspraak.getStatus();
			if (AfspraakStatus.GEPLAND.equals(status) || AfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType))
			{
				laatsteAfspraak.setRedenAfzeggen(RedenAfspraakAfzeggen.CLIENT_WIL_NIET_DEELNEMEN);
				afspraakService.afspraakAfzeggen(laatsteAfspraak, AfspraakStatus.GEANNULEERD_AFMELDEN, currentDateSupplier.getDateTime(), false);

				LOG.info("Afmelding: laatste intake afspraak is afgezegd");
			}
		}
	}

	private void nietVerzondenUitnodigingenVerwijderen(ColonScreeningRonde ronde)
	{
		ColonUitnodiging laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null && !laatsteUitnodiging.isVerstuurd())
		{
			List<ColonUitnodiging> uitnodigingen = ronde.getUitnodigingen();
			uitnodigingen.remove(laatsteUitnodiging);

			ColonUitnodiging nieuweLaatsteUitnodiging = null;
			for (ColonUitnodiging uitnodiging : uitnodigingen)
			{
				if (nieuweLaatsteUitnodiging == null || nieuweLaatsteUitnodiging.getUitnodigingsId() < uitnodiging.getUitnodigingsId())
				{
					nieuweLaatsteUitnodiging = uitnodiging;
				}
			}
			ronde.setLaatsteUitnodiging(nieuweLaatsteUitnodiging);
			hibernateService.delete(laatsteUitnodiging);
		}
	}

	private void cervixEenmaligAfmelden(CervixAfmelding afmelding)
	{
		CervixScreeningRonde ronde = afmelding.getScreeningRonde();
		cervixBaseScreeningrondeService.annuleerHerinnering(ronde);
		cervixBaseScreeningrondeService.annuleerNietVerstuurdeZAS(ronde);
		cervixBaseScreeningrondeService.annuleerUitstel(ronde);

		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			CervixBrief brief = uitnodiging.getBrief();
			if (!brief.isGegenereerd() || !uitnodiging.isVerstuurd())
			{
				brief.setTegenhouden(true);
				hibernateService.saveOrUpdate(brief);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void heraanmeldenZonderVervolg(Afmelding herAanTeMeldenAfmelding)
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

				ScreeningRonde ronde = getGeldigeRonde(herAanTeMeldenAfmelding);
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

						eenmaligHeraanmelden(ronde);
					}
				}
			}
			break;
		default:
			throw new IllegalStateException();
		}
	}

	private void definitiefHeraanmelden(Afmelding herAanTeMeldenAfmelding)
	{
		LOG.info("Definitief heraanmelden " + herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
			+ herAanTeMeldenAfmelding.getDossier().getClient().getId());

		herAanTeMeldenAfmelding.setHeraanmeldDatum(currentDateSupplier.getDate());
		herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
		herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(currentDateSupplier.getDate());

		Dossier dossier = herAanTeMeldenAfmelding.getDossier();
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

	private void eenmaligHeraanmelden(Afmelding herAanTeMeldenAfmelding, boolean implicieteHeraanmelding)
	{
		LOG.info("Eenmalig heraanmelden " + herAanTeMeldenAfmelding.getBevolkingsonderzoek().getAfkorting() + " voor client met id "
			+ herAanTeMeldenAfmelding.getScreeningRonde().getDossier().getClient().getId());

		eenmaligHeraanmelden(herAanTeMeldenAfmelding.getScreeningRonde());

		if (herAanTeMeldenAfmelding.getHeraanmeldStatus() != AanvraagBriefStatus.VERWERKT)
		{
			DateTime nu = currentDateSupplier.getDateTime();
			herAanTeMeldenAfmelding.setRondeHeropend(true);
			herAanTeMeldenAfmelding.setHeraanmeldDatum(nu.toDate());
			herAanTeMeldenAfmelding.setHeraanmeldStatus(AanvraagBriefStatus.VERWERKT);
			herAanTeMeldenAfmelding.setStatusHeraanmeldDatum(nu.toDate());
			herAanTeMeldenAfmelding.setImplicieteHeraanmelding(implicieteHeraanmelding);

			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}
	}

	private void eenmaligHeraanmelden(ScreeningRonde ronde)
	{
		ronde.setStatus(ScreeningRondeStatus.LOPEND);
		DateTime nu = currentDateSupplier.getDateTime();
		ronde.setStatusDatum(nu.plusMillis(200).toDate());
		ronde.setAangemeld(true);
		ronde.setAfgerondReden(null);

		hibernateService.saveOrUpdate(ronde);
	}

	private void vervolgHeraanmelden(Afmelding herAanTeMeldenAfmelding, Account account)
	{
		if (AanvraagBriefStatus.VERWERKT.equals(herAanTeMeldenAfmelding.getAfmeldingStatus()))
		{
			switch (herAanTeMeldenAfmelding.getBevolkingsonderzoek())
			{
			case COLON:
				colonVervolgHeraanmelden((ColonAfmelding) herAanTeMeldenAfmelding, account);
				break;
			case CERVIX:
				cervixVervolgHeraanmelden((CervixAfmelding) herAanTeMeldenAfmelding);
				break;
			case MAMMA:
				mammaVervolgHeraanmelden((MammaAfmelding) herAanTeMeldenAfmelding);
				break;
			default:
				throw new IllegalStateException();
			}
		}
	}

	private void mammaVervolgHeraanmelden(MammaAfmelding herAanTeMeldenAfmelding)
	{
		if (herAanTeMeldenAfmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			herAanTeMeldenAfmelding.setHeraanmeldBevestiging(
				briefService.maakMammaBrief(herAanTeMeldenAfmelding, BriefType.MAMMA_HERAANMELDING_BEVESTIGING, currentDateSupplier.getDateTime().plusMillis(200).toDate()));
			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}
	}

	private void colonVervolgHeraanmelden(ColonAfmelding herAanTeMeldenAfmelding, Account account)
	{
		ColonScreeningRonde ronde = getGeldigeRonde(herAanTeMeldenAfmelding);

		DateTime nu = currentDateSupplier.getDateTime();
		if (herAanTeMeldenAfmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			if (!ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(herAanTeMeldenAfmelding.getReden())
				&& BooleanUtils.isNotTrue(herAanTeMeldenAfmelding.getHeraanmeldingBevestigingsBriefTegenhouden()))
			{
				herAanTeMeldenAfmelding.setHeraanmeldBevestiging(
					briefService.maakColonBrief(herAanTeMeldenAfmelding, BriefType.COLON_HERAANMELDING_BEVESTIGING, nu.toDate()));
				hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
			}
		}

		ColonIntakeAfspraak afspraak = herAanTeMeldenAfmelding.getHeraanmeldingAfspraak();
		if (herAanTeMeldenAfmelding.getClientWilNieuweUitnodiging() && afspraak == null)
		{
			colonScreeningsrondeService.createNieuweUitnodiging(ronde, ColonUitnodigingCategorie.U4_2);
		}

		if (afspraak != null)
		{
			Client client = ronde.getDossier().getClient();
			afspraak.setClient(client);
			afspraak.setColonScreeningRonde(ronde);
			afspraak.setDatumLaatsteWijziging(nu.plusMillis(100).toDate());

			RoosterItem roosterItem = null;
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getHeraanmeldingAfspraakUitRooster()))
			{
				roosterItem = afspraakService.getRoosterBlokVoorAfspraak(afspraak);
				afspraak.setRoosterItem(roosterItem);
			}
			hibernateService.saveOrUpdate(afspraak);
			if (roosterItem != null)
			{
				roosterItem.getAfspraken().add(afspraak);
				hibernateService.saveOrUpdate(roosterItem);
			}

			ronde.getAfspraken().add(afspraak);
			ronde.setLaatsteAfspraak(afspraak);
			client.getAfspraken().add(afspraak);
			hibernateService.saveOrUpdate(client);
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

			BriefType afspraakBriefType = herAanTeMeldenAfmelding.getHeraanmeldingAfspraakBriefType();
			if (afspraakBriefType == null)
			{
				afspraakBriefType = BriefType.COLON_UITNODIGING_INTAKE;
			}
			ColonBrief brief = briefService.maakColonBrief(ronde, afspraakBriefType, nu.plusMillis(150).toDate());

			if (herAanTeMeldenAfmelding.getHeraanmeldingAfspraakBriefTegenhouden())
			{
				brief.setTegenhouden(true);
			}
			brief.setIntakeAfspraak(afspraak);
			hibernateService.saveOrUpdate(brief);
			LOG.info("Heraanmelden: nieuw afspraak aangemaakt");
		}

		if (ronde != null)
		{
			openUitnodigingService.afmeldingHeraanmeldingReactieOpOpenUitnodiging(herAanTeMeldenAfmelding, ronde, account);
			if (!herAanTeMeldenAfmelding.getClientWilNieuweUitnodiging())
			{
				ColonUitnodiging uitnodiging = ronde.getLaatsteUitnodiging();
				if (uitnodiging != null)
				{
					IFOBTTest gekoppeldeTest = uitnodiging.getGekoppeldeTest();
					IFOBTTestStatus ifobtTestStatus = IFOBTTestUtil.getActieveIFOBTTestStatusNaHeraanmelding(uitnodiging);
					if (ifobtTestStatus != null && gekoppeldeTest != null && gekoppeldeTest.getStatus().magWijzigenNaarStatus(ifobtTestStatus, gekoppeldeTest))
					{
						gekoppeldeTest.setStatus(ifobtTestStatus);
						gekoppeldeTest.setStatusDatum(nu.plusMillis(100).toDate());
						hibernateService.saveOrUpdate(gekoppeldeTest);
					}
				}
			}
		}
	}

	private void cervixVervolgHeraanmelden(CervixAfmelding herAanTeMeldenAfmelding)
	{
		if (herAanTeMeldenAfmelding.getType() == AfmeldingType.DEFINITIEF)
		{
			herAanTeMeldenAfmelding.setHeraanmeldBevestiging(
				briefService.maakCervixBrief(herAanTeMeldenAfmelding, BriefType.CERVIX_HERAANMELDING_BEVESTIGING, currentDateSupplier.getDateTime().plusMillis(200).toDate()));
			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}

		CervixScreeningRonde ronde = getGeldigeRonde(herAanTeMeldenAfmelding);
		if (ronde != null)
		{

			if (ronde.getMonsterHpvUitslag() == null || ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling().getHpvUitslag() == CervixHpvUitslag.POSITIEF
				&& ronde.getUitstrijkjeCytologieUitslag() == null || ronde.getUitnodigingVervolgonderzoek() != null && ronde.getUitstrijkjeVervolgonderzoekUitslag() == null)
			{
				if (cervixBaseScreeningrondeService.heeftUitnodigingMetMonsterInLabproces(ronde))
				{
					return;
				}
				CervixUitnodiging laatsteUitnodiging = getLaatstVerstuurdeUitnodiging(ronde, true);

				CervixBrief brief = null;
				boolean herinneren = true;
				if (laatsteUitnodiging != null)
				{
					CervixBrief laatsteUitnodigingBrief = laatsteUitnodiging.getBrief();
					brief = briefService.maakCervixBrief(ronde, laatsteUitnodigingBrief.getBriefType(), currentDateSupplier.getDateTime().plusMillis(200).toDate());
					brief.setHerdruk(laatsteUitnodigingBrief);
					hibernateService.saveOrUpdate(brief);

					herinneren = laatsteUitnodiging.getHerinneren();
				}
				else
				{
					brief = briefService.maakCervixBrief(ronde, BriefType.CERVIX_UITNODIGING, currentDateSupplier.getDateTime().plusMillis(200).toDate());
				}

				cervixFactory.maakUitnodiging(ronde, brief, herinneren, true);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public CervixUitnodiging getLaatstVerstuurdeUitnodiging(CervixScreeningRonde ronde, boolean inclusiefZas)
	{
		CervixUitnodiging laatsteUitnodiging = null;
		for (CervixUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			if (laatsteUitnodiging == null || laatsteUitnodiging.getId() < uitnodiging.getId())
			{
				if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE && uitnodiging.getBrief().isGegenereerd()
					|| inclusiefZas && uitnodiging.getMonsterType() == CervixMonsterType.ZAS && uitnodiging.getMonster() != null)
				{
					laatsteUitnodiging = uitnodiging;
				}
			}
		}
		return laatsteUitnodiging;
	}

	private ScreeningRonde getGeldigeRonde(Afmelding herAanTeMeldenAfmelding)
	{
		switch (herAanTeMeldenAfmelding.getBevolkingsonderzoek())
		{
		case COLON:
			return getGeldigeRonde((ColonAfmelding) herAanTeMeldenAfmelding);
		case CERVIX:
			return getGeldigeRonde((CervixAfmelding) herAanTeMeldenAfmelding);
		case MAMMA:
			return getGeldigeRonde((MammaAfmelding) herAanTeMeldenAfmelding);
		default:
			throw new IllegalStateException();
		}
	}

	private ColonScreeningRonde getGeldigeRonde(ColonAfmelding herAanTeMeldenAfmelding)
	{
		ColonScreeningRonde ronde = null;
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getRondeGesloten()))
			{
				ronde = herAanTeMeldenAfmelding.getScreeningRonde();
			}
			break;
		case DEFINITIEF:
			ronde = herAanTeMeldenAfmelding.getDossier().getLaatsteScreeningRonde();
			break;
		default:
			throw new IllegalStateException();
		}

		return ronde;
	}

	private CervixScreeningRonde getGeldigeRonde(CervixAfmelding herAanTeMeldenAfmelding)
	{
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			return herAanTeMeldenAfmelding.getScreeningRonde();
		case DEFINITIEF:
			CervixDossier dossier = herAanTeMeldenAfmelding.getDossier();
			Client client = dossier.getClient();
			CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null && DateUtil.compareBefore(herAanTeMeldenAfmelding.getHeraanmeldDatum(), dossier.getVolgendeRondeVanaf()))
			{
				return ronde;
			}
			else
			{
				CervixCISHistorie cisHistorie = dossier.getCisHistorie();
				if (cisHistorie != null && herAanTeMeldenAfmelding.equals(cisHistorie.getAfmelding()) && !cisHistorie.isHeeftUitslagInRonde0())
				{
					DateTime nu = currentDateSupplier.getDateTime();
					DateTime minimaleGeboortedatum = nu.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
					DateTime maximaleGeboortedatum = nu.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
					DateTime geboortedatum = new DateTime(client.getPersoon().getGeboortedatum());
					if (geboortedatum.isAfter(minimaleGeboortedatum) && geboortedatum.isBefore(maximaleGeboortedatum) && isHuidigeDatumBinnenRonde0(client))
					{
						cisHistorie.setScreeningRonde(cervixFactory.maakRonde(dossier));
					}
				}
			}
			return null;
		default:
			throw new IllegalStateException();
		}
	}

	private MammaScreeningRonde getGeldigeRonde(MammaAfmelding herAanTeMeldenAfmelding)
	{
		MammaScreeningRonde ronde = null;
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getRondeGesloten()))
			{
				ronde = herAanTeMeldenAfmelding.getScreeningRonde();
			}
			break;
		case DEFINITIEF:
			ronde = herAanTeMeldenAfmelding.getDossier().getLaatsteScreeningRonde();
			break;
		default:
			throw new IllegalStateException();
		}

		return ronde;
	}

	private boolean isHuidigeDatumBinnenRonde0(Client client)
	{
		String startdatumBMHKString = simplePreferenceService.getString(PreferenceKey.STARTDATUM_BMHK.name());
		LocalDate startdatumBMHK = LocalDate.parse(startdatumBMHKString, DateTimeFormatter.ofPattern("yyyyMMdd"));
		LocalDate geboorteDatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());

		CervixLeeftijdcategorie leeftijdsCategorie = CervixLeeftijdcategorie.getLeeftijdcategorie(geboorteDatum, currentDateSupplier.getLocalDateTime());
		LocalDate leeftijdsCategorieRondeDatum = geboorteDatum.plusYears(leeftijdsCategorie.getLeeftijd());

		return leeftijdsCategorieRondeDatum.isBefore(startdatumBMHK);
	}

	private void saveHandtekeningDocumentAfmelding(Client client, Afmelding afmelding)
	{
		try
		{
			UploadDocument handtekeningDocumentAfmelding = afmelding.getHandtekeningDocumentAfmelding();
			if (handtekeningDocumentAfmelding != null)
			{
				Long clientId = client.getId();
				switch (afmelding.getBevolkingsonderzoek())
				{
				case COLON:
					fileService.saveOrUpdateUploadDocument(handtekeningDocumentAfmelding, FileStoreLocation.COLON_AFMELDING, clientId);
					break;
				case CERVIX:
					fileService.saveOrUpdateUploadDocument(handtekeningDocumentAfmelding, FileStoreLocation.CERVIX_AFMELDING, clientId);
					break;
				case MAMMA:
					fileService.saveOrUpdateUploadDocument(handtekeningDocumentAfmelding, FileStoreLocation.MAMMA_AFMELDING, clientId);
					break;
				}
			}
		}
		catch (IOException e)
		{
			LOG.error("handtekening kon niet worden opgeslagen!", e.getMessage());
		}
	}

	@Override
	public Client getClientByAnummer(String anummer)
	{
		return clientDao.getClientByANummer(anummer);
	}

	@Override
	public boolean heeftClientIntakeConclusieMetBezwaar(String bsn)
	{
		return clientDao.heeftClientIntakeConclusieMetBezwaar(bsn);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveDocumentForClient(UploadDocument uploadDocument, Client client)
	{
		List<UploadDocument> documents = client.getDocuments();
		try
		{
			fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.CLIENT_DOCUMENTEN, client.getId());
			documents.add(uploadDocument);
			client.setDocuments(documents);
			hibernateService.saveOrUpdate(client);
		}
		catch (IOException e)
		{
			LOG.error("Er is een fout opgetreden! " + e.getMessage(), e);
		}

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteDocumentForClient(UploadDocument document, Client client)
	{
		fileService.deleteDocumentFromList(document, client.getDocuments());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void actiesNaUpdateWithGba(Client client)
	{
		if (client.getPersoon() != null)
		{
			GbaPersoon persoon = client.getPersoon();
			if (persoon.getOverlijdensdatum() != null)
			{
				alleProjectClientenInactiveren(client, ProjectInactiefReden.OVERLEDEN, null);
			}

			if (persoon.getDatumVertrokkenUitNederland() != null)
			{
				alleProjectClientenInactiveren(client, ProjectInactiefReden.VERTROKKEN_UIT_NEDERLAND, null);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void projectClientInactiveren(ProjectClient pClient, ProjectInactiefReden reden, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		if (pClient != null)
		{
			if (!reden.equals(ProjectInactiefReden.AFMELDING)
				|| reden.equals(ProjectInactiefReden.AFMELDING) && pClient.getProject().getExcludeerAfmelding().contains(bevolkingsonderzoek))
			{
				pClient.setActief(false);
				pClient.setProjectInactiefReden(reden);
				pClient.setProjectInactiefDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(pClient);

				DateFormat df = new SimpleDateFormat("dd-MM-yyyy");

				String melding = String.format("Project: %s, groep: %s, reden: %s",
					pClient.getProject().getNaam(),
					pClient.getGroep().getNaam(),
					reden.naam);

				logService.logGebeurtenis(LogGebeurtenis.PROJECTCLIENT_GEINACTIVEERD, pClient.getClient(), melding);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void alleProjectClientenInactiveren(Client client, ProjectInactiefReden projectInactiefReden, Bevolkingsonderzoek bvo)
	{
		List<ProjectClient> projectClienten = ProjectUtil.getHuidigeProjectClienten(client, currentDateSupplier.getDate(), false);
		if (projectClienten != null && projectClienten.size() > 0)
		{
			for (ProjectClient projectClient : projectClienten)
			{
				projectClientInactiveren(projectClient, projectInactiefReden, bvo);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void projectClientInactiveren(Client client, ProjectInactiefReden reden, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		ProjectClient pClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());
		if (pClient != null)
		{
			projectClientInactiveren(pClient, reden, bevolkingsonderzoek);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public String projectClientActiveren(ProjectClient projectClient)
	{

		if (ProjectUtil.getHuidigeProjectClient(projectClient.getClient(), currentDateSupplier.getDate()) != null)
		{
			return "ander.project.opgenomen";
		}
		projectClient.setActief(Boolean.TRUE);
		if (!isErEenUitnodigingAangemaaktInProjectPeriode(projectClient))
		{
			projectClient.setUitgenodigdInProjectPeriode(Boolean.TRUE);
		}
		hibernateService.saveOrUpdate(projectClient);
		return null;
	}

	private boolean isErEenUitnodigingAangemaaktInProjectPeriode(ProjectClient projectClient)
	{
		Date beginDatum = projectClient.getToegevoegd();
		List<ColonUitnodiging> uitnodigingen = clientDao.getAllColonUitnodigingenVanClientInPeriode(projectClient.getClient(), beginDatum, currentDateSupplier.getDate());
		return CollectionUtils.isNotEmpty(uitnodigingen);
	}

	@Override
	public List<Bevolkingsonderzoek> totWelkeBevolkingsonderzoekenHoortDezeClient(Client client)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<>();
		for (Bevolkingsonderzoek onderzoek : Bevolkingsonderzoek.values())
		{
			if (client == null || behoortTotDoelgroep(client, onderzoek))
			{
				onderzoeken.add(onderzoek);
			}
		}
		return onderzoeken;
	}

	@Override
	public boolean behoortTotDoelgroep(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		if (isClientOverleden(client))
		{
			return false;
		}
		else
		{
			DateTime nu = currentDateSupplier.getDateTime();
			MutableDateTime geboortedatum = new MutableDateTime(client.getPersoon().getGeboortedatum());

			switch (bevolkingsonderzoek)
			{
			case COLON:
				Integer minimaleLeeftijdColon = simplePreferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
				DateTime doelgroepColonAlsGeboortedatumVoor = nu.minusYears(minimaleLeeftijdColon);
				return geboortedatum.isBefore(doelgroepColonAlsGeboortedatumVoor) || client.getColonDossier().getLaatsteScreeningRonde() != null
					|| client.getColonDossier().getLaatsteAfmelding() != null;
			case CERVIX:
				boolean isVrouw = Geslacht.VROUW.equals(client.getPersoon().getGeslacht());
				DateTime minimaleGeboortedatum = nu.minusYears(CervixLeeftijdcategorie._70.getLeeftijd());
				DateTime maximaleGeboortedatum = nu.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
				return isVrouw && geboortedatum.isAfter(minimaleGeboortedatum) && geboortedatum.isBefore(maximaleGeboortedatum)
					|| client.getCervixDossier() != null && (client.getCervixDossier().getLaatsteScreeningRonde() != null
						|| client.getCervixDossier().getLaatsteAfmelding() != null);
			case MAMMA:
				Integer geboortejaar = geboortedatum.getYear();
				isVrouw = Geslacht.VROUW.equals(client.getPersoon().getGeslacht());
				Integer mammaMinimaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());
				Integer mammaMaximaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
				Integer minimaleGeboortejaar = nu.minusYears(mammaMaximaleLeeftijd).getYear() - 1;
				Integer maximaleGeboortejaar = nu.minusYears(mammaMinimaleLeeftijd).getYear();
				return isVrouw && geboortejaar >= minimaleGeboortejaar && geboortejaar <= maximaleGeboortejaar ||
					client.getMammaDossier() != null && (client.getMammaDossier().getLaatsteScreeningRonde() != null
						|| client.getMammaDossier().getLaatsteAfmelding() != null);

			default:
				return false;
			}
		}
	}

	@Override
	public boolean isClientOverleden(Client client)
	{
		return client != null && client.getPersoon() != null && client.getPersoon().getOverlijdensdatum() != null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public GbaVraag vraagGbaGegevensOpnieuwAan(Client client, Account account, RedenOpnieuwAanvragenClientgegevens reden)
	{
		GbaVraag gbaVraag = new GbaVraag();
		gbaVraag.setClient(client);
		gbaVraag.setDatum(currentDateSupplier.getDate());
		gbaVraag.setVraagType(GbaVraagType.VERWIJDER_INDICATIE);
		gbaVraag.setReden(reden);
		gbaVraag.setReactieOntvangen(false);

		hibernateService.saveOrUpdate(gbaVraag);

		client.setGbaStatus(GbaStatus.INDICATIE_VERWIJDERD);
		hibernateService.saveOrUpdate(client);
		String melding = "Reden: ";
		if (account != null)
		{
			melding += reden.getNaam();
		}
		else
		{
			melding += "Retourzending";
		}
		logService.logGebeurtenis(LogGebeurtenis.GBA_GEGEVENS_OPNIEUW_AANGEVRAAGD, getScreeningOrganisatieVan(client), account, client, melding);
		return gbaVraag;
	}

	@Override
	public List<Instelling> getScreeningOrganisatieVan(Client client)
	{
		if (client != null && client.getPersoon().getGbaAdres() != null && client.getPersoon().getGbaAdres().getGbaGemeente() != null
			&& client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie() != null)
		{
			return Arrays.asList(client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		}
		return new ArrayList<>();
	}

	@Override
	public boolean isTijdelijkeAdresNuActueel(GbaPersoon persoon)
	{
		return AdresUtil.isTijdelijkAdres(persoon, currentDateSupplier.getDateTime());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateTijdelijkGbaAdres(Client client, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "Gewijzigd.";
		GbaPersoon persoon = client.getPersoon();
		TijdelijkGbaAdres tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
		if (tijdelijkGbaAdres != null && tijdelijkGbaAdres.getId() == null)
		{
			melding = "Aangemaakt.";
		}
		logService.logGebeurtenis(LogGebeurtenis.GBA_TIJDELIJK_ADRES, ingelogdeGebruiker, client, melding);
		hibernateService.saveOrUpdate(persoon);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderTijdelijkGbaAdres(Client client, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "Handmatig verwijderd.";
		GbaPersoon persoon = client.getPersoon();
		TijdelijkGbaAdres tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
		persoon.setTijdelijkGbaAdres(null);
		hibernateService.delete(tijdelijkGbaAdres);
		logService.logGebeurtenis(LogGebeurtenis.GBA_TIJDELIJK_ADRES, ingelogdeGebruiker, client, melding);
		hibernateService.saveOrUpdate(persoon);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void afmelden(Client client, Afmelding afmelding, Account account)
	{
		if (afmelding.getType() == AfmeldingType.DEFINITIEF && afmelding.getAfmeldingStatus() == null)
		{
			definitieveAfmeldingAanvragen(client, afmelding, false, account);
		}
		else
		{
			afmeldenZonderVervolg(client, afmelding, true, account);
			vervolgAfmelden(afmelding);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void heraanmelden(Afmelding herAanTeMeldenAfmelding, Account account)
	{
		if (herAanTeMeldenAfmelding != null)
		{
			heraanmeldenZonderVervolg(herAanTeMeldenAfmelding);
			vervolgHeraanmelden(herAanTeMeldenAfmelding, account);
			logService.logGebeurtenis(LogGebeurtenis.HERAANMELDEN, account, AfmeldingUtil.getClientFromAfmelding(herAanTeMeldenAfmelding),
				"Type: " + herAanTeMeldenAfmelding.getType().name().toLowerCase(),
				herAanTeMeldenAfmelding.getBevolkingsonderzoek());
		}
	}

	@Override
	public String valideerBriefkenmerk(String briefkenmerk, Client zoekClient)
	{
		String errorString = null;
		try
		{

			briefkenmerk = briefkenmerk.replaceAll(" ", "").toUpperCase();
			if (briefkenmerk.startsWith("K"))
			{
				Client client = getClientMetBriefkenmerk(briefkenmerk);

				boolean heeftBriefPersoon = client != null && client.getPersoon() != null;
				boolean heeftZoekClientPersoon = zoekClient != null && zoekClient.getPersoon() != null;
				boolean kloptIngevoerdeGeboortedatum = heeftBriefPersoon && heeftZoekClientPersoon
					&& zoekClient.getPersoon().getGeboortedatum().equals(client.getPersoon().getGeboortedatum());
				if (heeftBriefPersoon && heeftZoekClientPersoon)
				{
					GbaPersoon zoekPersoon = zoekClient.getPersoon();
					GbaPersoon briefPersoon = client.getPersoon();

					boolean kloptIngevoerdeBsn = zoekPersoon.getBsn() == null || zoekPersoon.getBsn() != null && zoekPersoon.getBsn().equals(briefPersoon.getBsn());

					BagAdres zoekAdres = zoekPersoon.getGbaAdres();
					boolean kloptIngevoerdePostcodeEnHuisNr = zoekAdres == null || zoekAdres.getPostcode() == null && zoekAdres.getHuisnummer() == null
						|| zoekAdres.getPostcode() != null && zoekAdres.getHuisnummer() != null
							&& zoekAdres.getPostcode().equals(briefPersoon.getGbaAdres().getPostcode())
							&& zoekAdres.getHuisnummer().equals(briefPersoon.getGbaAdres().getHuisnummer());

					if (kloptIngevoerdeGeboortedatum && kloptIngevoerdeBsn && kloptIngevoerdePostcodeEnHuisNr)
					{
						return null;
					}
				}
				errorString = "error.briefkenmerk.combi.niet.gevonden";
			}
			else
			{
				errorString = "error.onjuiste.briefkenmerkformat";
			}
		}
		catch (NumberFormatException e)
		{
			errorString = "error.onjuiste.briefkenmerk";
		}
		return errorString;
	}

	@Override
	public Client getClientMetBriefkenmerk(String briefkenmerk)
	{
		Client client = null;

		briefkenmerk = briefkenmerk.replaceAll(" ", "").toUpperCase();
		if (briefkenmerk.startsWith("KU"))
		{ 
			briefkenmerk = briefkenmerk.substring(2);
			if (NumberUtils.isCreatable("0x" + briefkenmerk))
			{
				Map<String, Object> parameters = new HashMap<>();
				parameters.put("uitnodigingsId", Long.parseLong(briefkenmerk, 16));
				ColonUitnodiging uitnodiging = hibernateService.getUniqueByParameters(ColonUitnodiging.class, parameters);
				if (uitnodiging != null)
				{
					client = uitnodiging.getScreeningRonde().getDossier().getClient();
				}
			}
		}
		else
		{ 
			briefkenmerk = briefkenmerk.substring(1);
			if (NumberUtils.isCreatable("0x" + briefkenmerk))
			{
				ClientBrief brief = hibernateService.get(ClientBrief.class, Long.parseLong(briefkenmerk, 16));
				if (brief != null)
				{
					client = brief.getClient();
				}
			}
		}
		if (client != null && !client.getGbaStatus().equals(GbaStatus.BEZWAAR) && !client.getGbaStatus().equals(GbaStatus.AFGEVOERD))
		{
			return client;
		}
		return null;
	}

	@Override
	public CentraleEenheid bepaalCe(Client client)
	{
		MammaStandplaatsPeriode periode = baseStandplaatsService.getEerstvolgendeStandplaatsPeriode(baseStandplaatsService.getStandplaatsMetPostcode(client));
		if (periode == null && client.getMammaDossier().getLaatsteScreeningRonde() != null)
		{
			MammaScreeningRonde screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getLaatsteUitnodiging() != null && screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak() != null)
			{
				periode = screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak().getStandplaatsPeriode();
			}
		}

		if (periode != null)
		{
			return (CentraleEenheid) HibernateHelper.deproxy(periode.getScreeningsEenheid().getBeoordelingsEenheid().getParent());
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean vervangAfmeldingDocument(UploadDocument nieuwDocument, Afmelding afmelding, UploadDocument huidigDocument, ClientBrief brief, Account loggedInAccount)
	{
		afmelding.setHandtekeningDocumentAfmelding(null);
		fileService.delete(huidigDocument, true);

		afmelding.setHandtekeningDocumentAfmelding(nieuwDocument);
		try
		{
			fileService.saveOrUpdateUploadDocument(nieuwDocument, FileStoreLocation.getAfmelding(brief.getBevolkingsonderzoek()), brief.getClient().getId());
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
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean vervangHeraanmeldingDocument(UploadDocument nieuwDocument, Afmelding afmelding, UploadDocument huidigDocument, ClientBrief brief, Account loggedInAccount)
	{
		afmelding.setHandtekeningDocumentHeraanmelding(null);
		fileService.delete(huidigDocument, true);

		afmelding.setHandtekeningDocumentHeraanmelding(nieuwDocument);
		try
		{
			fileService.saveOrUpdateUploadDocument(nieuwDocument, FileStoreLocation.getHeraanmelding(brief.getBevolkingsonderzoek()), brief.getClient().getId());
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
	public String getGbaPostcode(Client client)
	{
		return client.getPersoon().getTijdelijkGbaAdres() != null ? client.getPersoon().getTijdelijkGbaAdres().getPostcode()
			: client.getPersoon().getGbaAdres().getPostcode();
	}

	@Override
	public boolean isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(UploadDocument handtekeningBrief, String handtekeningProperty)
	{
		return clientDao.countUsedColonHandtekeningBrief(handtekeningBrief, handtekeningProperty) > 1;
	}

	@Override
	public void heraanmeldenAlsClientAfgemeldIs(MammaDossier dossier)
	{
		if (AfmeldingUtil.isAfgemeld(dossier))
		{
			MammaAfmelding afmelding = AfmeldingUtil.getLaatsteAfmelding(dossier.getLaatsteScreeningRonde(), dossier);
			heraanmeldenZonderVervolg(afmelding);
		}
	}
}
