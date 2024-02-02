package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.colon.AfspraakDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.planning.model.IParticipant;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.Location;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

@Slf4j
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Service
public class AfspraakServiceImpl implements AfspraakService
{

	@Autowired
	private AfspraakDao afspraakDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonHuisartsBerichtService berichtenService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Override
	public List<IAppointment> getAppointmentsMedewerker(IParticipant medewerker, Date start, Date end)
	{
		if (medewerker instanceof Client)
		{
			Client client = (Client) medewerker;
			Afspraak filter = new Afspraak();
			filter.setClient(client);
			filter.setStatus(AfspraakStatus.GEPLAND);

			return new ArrayList<>(afspraakDao.getAfspraken(start, end, filter, null, AfspraakStatus.VOOR_AGENDA));
		}

		return null;
	}

	@Override
	public List<IAppointment> getAppointmentsApparaat(IParticipant apparaat, Date start, Date end)
	{

		return null;
	}

	@Override
	public List<? extends IAppointment> getAppointments(Location locatie, Date start, Date end)
	{
		return getAppointments(locatie, start, end, false);
	}

	@Override
	public List<? extends IAppointment> getAlgemeneUitsluitingen(Date start, Date end)
	{
		return new ArrayList<>();
	}

	@Override
	public List<IAppointment> getAppointments(Location locatie, Date start, Date end, boolean showSchedule)
	{
		List<Location> locaties = new ArrayList<>();
		locaties.add(locatie);

		return getAppointments(locaties, start, end, showSchedule);
	}

	@Override
	public List<IAppointment> getAppointments(List<Location> locaties, Date start, Date end, boolean showSchedule)
	{
		Afspraak filter = new Afspraak();

		filter.setStatus(AfspraakStatus.GEPLAND);
		List<IAppointment> returnValues = new ArrayList<>(afspraakDao.getAfspraken(start, end, filter, locaties, AfspraakStatus.VOOR_AGENDA));

		if (showSchedule)
		{
			RoosterItem roosterFilter = new RoosterItem();

			returnValues.addAll(afspraakDao.getAfspraken(start, end, roosterFilter, locaties, null));
		}

		return returnValues;

	}

	@Override
	public List<Afspraak> getAppointments(Client client)
	{
		Afspraak filter = new Afspraak();
		filter.setClient(client);
		filter.setStatus(AfspraakStatus.GEPLAND);

		return afspraakDao.getAfspraken(currentDateSupplier.getDate(), null, filter, null, AfspraakStatus.VOOR_AGENDA);
	}

	@Override
	public List<Afspraak> getHistorischeAppointments(Client client)
	{
		Afspraak filter = new Afspraak();
		filter.setClient(client);

		return afspraakDao.getAfspraken(null, currentDateSupplier.getDate(), filter, null, null);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void annuleerAfspraak(Afspraak afspraak, Account account, AfspraakStatus status, boolean communicatieTegenhouden)
	{
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		Client client = afspraak.getClient();
		String afzegReden = "";
		if (afspraak instanceof ColonIntakeAfspraak)
		{
			ColonIntakeAfspraak intakeAfspraak = (ColonIntakeAfspraak) afspraak;
			afspraakAfzeggen(intakeAfspraak, status, nu, communicatieTegenhouden);

			ColonScreeningRonde screeningRonde = intakeAfspraak.getColonScreeningRonde();
			if (screeningRonde.getOpenUitnodiging() == null) 

			{
				if (screeningRonde.getStatus() == ScreeningRondeStatus.LOPEND)
				{
					screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
					screeningRonde.setStatusDatum(DateUtil.toUtilDate(nu));
				}
			}
			else
			{
				OpenUitnodiging uitnodiging = screeningRonde.getOpenUitnodiging();
				uitnodiging.setUitslag(null);
				uitnodiging.setDatum(null);
				uitnodiging.setAfspraak(null);
				hibernateService.saveOrUpdate(uitnodiging);
			}
		}
		afspraakDao.saveOrUpdate(afspraak);
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		String melding = String.format("Intake afspraak van %1$s in %2$s van %3$s afgezegd%4$s", format.format(afspraak.getStartTime()), afspraak.getLocation().getName(),
			afspraak.getLocation().getColoscopieCentrum().getNaam(), afzegReden);
		logService.logGebeurtenis(LogGebeurtenis.AFSPRAAK_AFGEZEGD, account, client, melding, Bevolkingsonderzoek.COLON);

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void afspraakAfzeggen(ColonIntakeAfspraak afspraak, AfspraakStatus status, LocalDateTime nu, boolean communicatieTegenhouden)
	{
		setAfspraakStatus(afspraak, status);
		afspraak.setAfzegDatum(DateUtil.toUtilDate(nu.plus(100, ChronoUnit.MILLIS)));
		hibernateService.saveOrUpdate(afspraak);

		ColonScreeningRonde screeningRonde = afspraak.getColonScreeningRonde();
		dossierBaseService.setDatumVolgendeUitnodiging(screeningRonde.getDossier(), ColonUitnodigingsintervalType.GEANNULEERDE_INTAKE_AFSPRAAK);

		if (!AfspraakStatus.GEANNULEERD_OPEN_UITNODIGING.equals(status))
		{
			Client client = afspraak.getClient();
			if (!client.getPersoon().getGbaAdres().getGbaGemeente().getCode().equals(Gemeente.RNI_CODE))
			{
				ColonBrief colonBrief = briefService.maakBvoBrief(screeningRonde, BriefType.COLON_INTAKE_AFMELDING, DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS)));
				hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(colonBrief, communicatieTegenhouden));

				if (!communicatieTegenhouden)
				{
					MailMergeContext context = new MailMergeContext();
					context.setClient(client);
					context.setIntakeAfspraak(afspraak);
					if (screeningRonde.getLaatsteUitnodiging() != null)
					{
						context.setColonUitnodiging(screeningRonde.getLaatsteUitnodiging());
					}

					berichtenService.verstuurColonHuisartsBericht(client, screeningRonde, HuisartsBerichtType.ANNULEREN_INTAKEAFSPRAAK, context);
				}

			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setAfspraakStatus(Afspraak afspraak, AfspraakStatus status)
	{
		afspraak.setStatus(status);
		if (!AfspraakStatus.GEPLAND.equals(status) && !AfspraakStatus.UITGEVOERD.equals(status))
		{
			RoosterItem roosterItem = afspraak.getRoosterItem();
			if (roosterItem != null)
			{
				roosterItem.getAfspraken().remove(afspraak);
				hibernateService.saveOrUpdate(roosterItem);
				afspraak.setRoosterItem(null);
			}
		}
	}

	@Override
	public List<? extends IAppointment> getAppointmentsMetFilter(Location locatie, Date startTijd, Date eindTijd, AbstractAppointment filter)
	{
		List<Location> locaties = null;
		if (locatie != null)
		{
			locaties = new ArrayList<>();
			locaties.add(locatie);
		}

		return afspraakDao.getAfspraken(startTijd, eindTijd, filter, locaties, null);
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColoscopieCentrum coloscopieCentrum, long first, long count,
		String property, boolean ascending)
	{
		return afspraakDao.getAfsprakenVoorColoscopiecentrum(zoekObject, coloscopieCentrum, currentDateSupplier.getLocalDate(), first, count, property, ascending);
	}

	@Override
	public long countAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColoscopieCentrum coloscopieCentrum)
	{
		return afspraakDao.countAfsprakenVoorColoscopiecentrum(zoekObject, coloscopieCentrum, currentDateSupplier.getLocalDate());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verplaatsAfspraak(ColonIntakeAfspraak nieuweAfspraak, Account account, BriefType briefType, boolean briefTegenhouden, boolean binnenRooster,
		boolean verwezenMedischeRedenenDoorInfolijn)
	{
		ColonScreeningRonde colonScreeningRonde = nieuweAfspraak.getColonScreeningRonde();

		ColonIntakeAfspraak laatsteAfspraak = colonScreeningRonde.getLaatsteAfspraak();
		if (nieuweAfspraak == null || nieuweAfspraak.getId() != null || nieuweAfspraak.equals(laatsteAfspraak) || laatsteAfspraak.getStatus().equals(AfspraakStatus.VERPLAATST))
		{

			return;
		}
		colonScreeningRonde.getAfspraken().add(nieuweAfspraak);

		laatsteAfspraak.setNieuweAfspraak(nieuweAfspraak);
		nieuweAfspraak.setOudeAfspraak(laatsteAfspraak);
		setStatus(nieuweAfspraak, account, laatsteAfspraak, verwezenMedischeRedenenDoorInfolijn);

		Client client = laatsteAfspraak.getClient();
		client.getAfspraken().add(nieuweAfspraak);

		colonScreeningRonde.setLaatsteAfspraak(nieuweAfspraak);
		RoosterItem roosterItem = null;
		if (binnenRooster)
		{
			roosterItem = getRoosterBlokVoorAfspraak(nieuweAfspraak);
			nieuweAfspraak.setRoosterItem(roosterItem);
		}
		hibernateService.saveOrUpdate(nieuweAfspraak);
		hibernateService.saveOrUpdate(laatsteAfspraak);
		hibernateService.saveOrUpdate(colonScreeningRonde);
		hibernateService.saveOrUpdate(client);
		if (roosterItem != null)
		{
			roosterItem.getAfspraken().add(nieuweAfspraak);
			hibernateService.saveOrUpdate(roosterItem);
		}

		if (briefType == null)
		{
			briefType = BriefType.COLON_INTAKE_GEWIJZIGD;
		}
		ColonBrief brief = briefService.maakBvoBrief(colonScreeningRonde, briefType);
		brief.setIntakeAfspraak(nieuweAfspraak);
		if (briefTegenhouden)
		{
			hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
		}
		hibernateService.saveOrUpdate(brief);

		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy HH:mm");
		String melding = String.format("Verplaatst van %1$s in %2$s van %3$s naar %4$s in %5$s van %6$s", format.format(laatsteAfspraak.getStartTime()),
			laatsteAfspraak.getLocation().getName(), laatsteAfspraak.getLocation().getColoscopieCentrum().getNaam(), format.format(nieuweAfspraak.getStartTime()),
			nieuweAfspraak.getLocation().getName(), nieuweAfspraak.getLocation().getColoscopieCentrum().getNaam());
		if (!briefType.equals(BriefType.COLON_INTAKE_GEWIJZIGD))
		{
			melding += "; afwijkende brief";
		}
		logService.logGebeurtenis(LogGebeurtenis.AFSPRAAK_VERPLAATST, account, client, melding, Bevolkingsonderzoek.COLON);

		if (!briefTegenhouden)
		{
			verstuurWijzigingsberichtNaarHA(nieuweAfspraak, laatsteAfspraak, client);
		}
	}

	private void setStatus(ColonIntakeAfspraak nieuweAfspraak, Account account, ColonIntakeAfspraak laatsteAfspraak, boolean verwezenMedischeRedenenDoorInfolijn)
	{
		account = (Account) HibernateHelper.deproxy(account);
		boolean lijktOpEenVerwijzing = laatsteAfspraak.getConclusie() == null
			&& DateUtil.compareBefore(laatsteAfspraak.getStartTime(), currentDateSupplier.getDate())
			&& account instanceof InstellingGebruiker
			&& !laatsteAfspraak.getLocation().getColoscopieCentrum().equals(nieuweAfspraak.getLocation().getColoscopieCentrum());
		if (lijktOpEenVerwijzing && verwezenMedischeRedenenDoorInfolijn)
		{
			ColonConclusie conclusie = new ColonConclusie();
			conclusie.setType(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM);
			conclusie.setDatum(currentDateSupplier.getDate());
			conclusie.setDoorverwijzingBevestigd(false);
			conclusie.setInstellingGebruiker((InstellingGebruiker) account);
			hibernateService.saveOrUpdate(conclusie);
			laatsteAfspraak.setConclusie(conclusie);
			setAfspraakStatus(laatsteAfspraak, AfspraakStatus.UITGEVOERD);
		}
		else
		{
			setAfspraakStatus(laatsteAfspraak, AfspraakStatus.VERPLAATST);
		}
	}

	protected void verstuurWijzigingsberichtNaarHA(ColonIntakeAfspraak nieuweAfspraak, ColonIntakeAfspraak laatsteAfspraak, Client client)
	{

		MailMergeContext context = new MailMergeContext();
		context.setClient(client);
		context.setIntakeAfspraak(nieuweAfspraak);
		context.setVorigeIntakeAfspraak(laatsteAfspraak);

		if (nieuweAfspraak != null && nieuweAfspraak.getColonScreeningRonde() != null && nieuweAfspraak.getColonScreeningRonde().getLaatsteUitnodiging() != null)
		{
			context.setColonUitnodiging(nieuweAfspraak.getColonScreeningRonde().getLaatsteUitnodiging());
		}
		HuisartsBerichtType berichtType = HuisartsBerichtType.WIJZIGING_INTAKEAFSPRAAK;
		if (laatsteAfspraak == null)
		{
			if (nieuweAfspraak != null && nieuweAfspraak.getColonScreeningRonde() != null && nieuweAfspraak.getColonScreeningRonde().getOpenUitnodiging() != null)
			{
				berichtType = HuisartsBerichtType.INTAKE_NA_OPEN_UITNODIGING;
			}
			else
			{
				berichtType = HuisartsBerichtType.ONGUNSTIGE_UITSLAG;
			}
		}
		ColonScreeningRonde ronde = client.getColonDossier().getLaatsteScreeningRonde();
		try
		{
			berichtenService.verstuurColonHuisartsBericht(client, ronde, berichtType, context);
		}
		catch (Exception e)
		{
			LOG.error("Huisarts Bericht kon niet worden aangemaakt. ", e);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakNieuweAfspraak(Client client, ColonIntakeAfspraak nieuweAfspraak, boolean briefTegenhouden, boolean binnenRooster,
		BriefType briefType, Account account)
	{
		OpenUitnodiging openUitnodiging;
		ColonDossier colonDossier = client.getColonDossier();
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		nieuweAfspraak.setColonScreeningRonde(laatsteScreeningRonde);
		nieuweAfspraak.setClient(client);
		nieuweAfspraak.setDatumLaatsteWijziging(DateUtil.toUtilDate(nu.plus(100, ChronoUnit.MILLIS)));
		RoosterItem roosterItem;
		if (binnenRooster)
		{
			roosterItem = getRoosterBlokVoorAfspraak(nieuweAfspraak);
			nieuweAfspraak.setRoosterItem(roosterItem);
		}
		else
		{
			roosterItem = getVrijRoosterBlokVoorAfspraak(nieuweAfspraak);
			if (roosterItem != null)
			{
				nieuweAfspraak.setRoosterItem(roosterItem);
			}
		}
		hibernateService.saveOrUpdate(nieuweAfspraak);

		boolean heefAlOpenUitnodigingsBriefGehad = false;
		for (ColonBrief brief : laatsteScreeningRonde.getBrieven())
		{
			if (BriefType.COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING.equals(brief.getBriefType()))
			{
				heefAlOpenUitnodigingsBriefGehad = true;
				break;
			}
		}

		openUitnodiging = laatsteScreeningRonde.getOpenUitnodiging();
		if (openUitnodiging != null && openUitnodiging.getUitslag() == null)
		{
			openUitnodiging.setUitslag(OpenUitnodigingUitslag.INTAKE_AFSPRAAK);
			openUitnodiging.setAfspraak(nieuweAfspraak);
			hibernateService.saveOrUpdate(openUitnodiging);
			logService.logGebeurtenis(LogGebeurtenis.OPEN_UITNODIGING_REACTIE, account, client, "Client heeft een nieuwe afspraak gemaakt", Bevolkingsonderzoek.COLON);
		}

		if (roosterItem != null)
		{
			roosterItem.getAfspraken().add(nieuweAfspraak);
			hibernateService.saveOrUpdate(roosterItem);
		}

		laatsteScreeningRonde.getAfspraken().add(nieuweAfspraak);
		ColonIntakeAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
		laatsteScreeningRonde.setLaatsteAfspraak(nieuweAfspraak);
		if (heeftOnafgerondeVerwijzingOmMedischeRedenen(laatsteAfspraak))
		{
			laatsteAfspraak.setNieuweAfspraak(nieuweAfspraak);
			nieuweAfspraak.setOudeAfspraak(laatsteAfspraak);
			hibernateService.saveOrUpdateAll(nieuweAfspraak, laatsteAfspraak);
		}
		client.getAfspraken().add(nieuweAfspraak);
		hibernateService.saveOrUpdate(client);
		ColonBrief brief;
		Date creatieDatumColonBrief = DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS));
		if (openUitnodiging != null && !heefAlOpenUitnodigingsBriefGehad)
		{
			brief = briefService.maakBvoBrief(laatsteScreeningRonde, BriefType.COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING, creatieDatumColonBrief);
			brief.setIntakeAfspraak(nieuweAfspraak);
		}
		else
		{
			if (briefType != null)
			{
				brief = briefService.maakBvoBrief(laatsteScreeningRonde, briefType, creatieDatumColonBrief);
				brief.setIntakeAfspraak(nieuweAfspraak);
			}
			else
			{
				brief = briefService.maakBvoBrief(laatsteScreeningRonde, BriefType.COLON_INTAKE_GEWIJZIGD, creatieDatumColonBrief);
				brief.setIntakeAfspraak(nieuweAfspraak);
			}
		}
		if (briefTegenhouden)
		{
			hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
		}
		hibernateService.saveOrUpdate(brief);

		laatsteScreeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
		laatsteScreeningRonde.setStatusDatum(DateUtil.toUtilDate(nu.plus(200, ChronoUnit.MILLIS)));
		laatsteScreeningRonde.setAfgerondReden(null);
		hibernateService.saveOrUpdate(laatsteScreeningRonde);

		dossierBaseService.setDatumVolgendeUitnodiging(laatsteScreeningRonde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

		verstuurWijzigingsberichtNaarHA(nieuweAfspraak, laatsteAfspraak, client);
	}

	@Override
	public void verzendHuisartsBerichtOpnieuw(Client client, Account account)
	{
		ColonScreeningRonde laatsteScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
		ColonIntakeAfspraak colonIntakeAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
		HuisartsBerichtType berichtType = HuisartsBerichtType.ONGUNSTIGE_UITSLAG;

		switch (colonIntakeAfspraak.getStatus())
		{
		case GEPLAND:
		case UITGEVOERD:
			if (colonIntakeAfspraak.getConclusie() != null && ColonConclusieType.NO_SHOW.equals(colonIntakeAfspraak.getConclusie().getType()))
			{
				berichtType = HuisartsBerichtType.NO_SHOW_INTAKE;
			}
			break;
		case GEANNULEERD_AFMELDEN:
		case GEANNULEERD_VIA_INFOLIJN:
		case GEANNULEERD_CLIENT:
		case GEANNULEERD_OPEN_UITNODIGING:
		case GEANNULEERD_OVERLIJDEN:
			berichtType = HuisartsBerichtType.ANNULEREN_INTAKEAFSPRAAK;
			break;
		}

		MailMergeContext context = new MailMergeContext();
		context.setClient(client);
		context.setIntakeAfspraak(colonIntakeAfspraak);

		try
		{
			berichtenService.verstuurColonHuisartsBericht(client, laatsteScreeningRonde, laatsteScreeningRonde.getColonHuisarts(), berichtType, context, true);
		}
		catch (Exception e)
		{
			LOG.error("Huisarts Bericht kon niet worden aangemaakt. ", e);
		}
	}

	@Override
	public boolean magWijzigenAfzeggen(Afspraak afspraak)
	{
		ColonConclusieType colonConclusieType = null;
		boolean isLaatsteRonde = false;
		afspraak = (Afspraak) HibernateHelper.deproxy(afspraak);
		boolean heeftVerslagenInLaatsteRonde = false;
		boolean rondeAfspraakIsLopend = true;
		if (afspraak instanceof ColonIntakeAfspraak)
		{
			ColonIntakeAfspraak colonIntakeAfspraak = (ColonIntakeAfspraak) afspraak;
			ColonConclusie conclusie = colonIntakeAfspraak.getConclusie();
			ColonScreeningRonde ronde = colonIntakeAfspraak.getColonScreeningRonde();
			isLaatsteRonde = ronde.equals(ronde.getDossier().getLaatsteScreeningRonde());
			if (isLaatsteRonde)
			{
				heeftVerslagenInLaatsteRonde = ColonScreeningRondeUtil.heeftAfgerondeVerslag(ronde);
			}
			if (conclusie != null)
			{
				colonConclusieType = conclusie.getType();
			}
			rondeAfspraakIsLopend = ScreeningRondeStatus.LOPEND.equals(ronde.getStatus());
		}

		AfspraakStatus status = afspraak.getStatus();
		return isLaatsteRonde && !heeftVerslagenInLaatsteRonde && rondeAfspraakIsLopend
			&& (AfspraakStatus.GEPLAND.equals(status) || AfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType));
	}

	@Override
	public boolean magNieuweAfspraakMaken(Client client)
	{
		ColonDossier colonDossier = client.getColonDossier();
		boolean isDossierAangemeld = colonDossier.getAangemeld();

		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		boolean isLaatsteRondeGeldigEnAangemeld = ColonScreeningRondeUtil.isLaatsteScreeningRondGeldigEnAangemeld(laatsteScreeningRonde);

		if (isDossierAangemeld && isLaatsteRondeGeldigEnAangemeld && !ColonScreeningRondeUtil.heeftBuitenDoelgroepBrief(laatsteScreeningRonde)
			&& !ColonScreeningRondeUtil.heeftAfgerondeVerslag(laatsteScreeningRonde))
		{
			ColonIntakeAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
			boolean isErEenOpenUitnodiging = laatsteScreeningRonde.getOpenUitnodiging() != null;
			boolean isErEenOpenUitnodigingReactie = isErEenOpenUitnodiging && laatsteScreeningRonde.getOpenUitnodiging().getUitslag() != null;
			if (laatsteAfspraak != null)
			{
				ColonConclusie conclusie = laatsteAfspraak.getConclusie();
				boolean clientWilAndereIntakeLocatie = AfspraakStatus.UITGEVOERD.equals(laatsteAfspraak.getStatus()) && conclusie != null
					&& ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE.equals(conclusie.getType());

				boolean clientWordtDoorverwezenMedischeReden = heeftOnafgerondeVerwijzingOmMedischeRedenen(laatsteAfspraak);

				return clientWilAndereIntakeLocatie
					|| AfspraakStatus.isGeannuleerd(laatsteAfspraak.getStatus()) && !isErEenOpenUitnodigingReactie
					|| clientWordtDoorverwezenMedischeReden;
			}

			else if (!isErEenOpenUitnodigingReactie)
			{
				if (GbaStatus.AFGEVOERD.equals(client.getGbaStatus()))
				{
					return false;
				}
				if (isErEenOpenUitnodiging)
				{
					return true;
				}
				IFOBTTest ifobtTest = laatsteScreeningRonde.getLaatsteIFOBTTest();
				boolean isIfobtUitslagOngunstig = FITTestUtil.isOngunstig(ifobtTest);

				if (!isIfobtUitslagOngunstig && !FITTestUtil.isGunstig(ifobtTest))
				{

					ifobtTest = null;
					List<ColonScreeningRonde> rondes = new ArrayList<>(colonDossier.getScreeningRondes());
					Collections.sort(rondes, new PropertyComparator<>("id", false, true));
					for (ColonScreeningRonde ronde : rondes)
					{
						if (!ronde.equals(laatsteScreeningRonde))
						{
							Date vroegsteAnalyseDatum = null;
							for (IFOBTTest test : ronde.getIfobtTesten())
							{
								if (FITTestUtil.isOngunstig(test) && (vroegsteAnalyseDatum == null || vroegsteAnalyseDatum.after(test.getAnalyseDatum())))
								{
									ifobtTest = test;
									isIfobtUitslagOngunstig = true;
									vroegsteAnalyseDatum = test.getAnalyseDatum();
								}
							}
						}
					}

					if (isIfobtUitslagOngunstig)
					{
						for (Afspraak afspraak : colonDossier.getClient().getAfspraken())
						{
							if (afspraak.getIngevoerd() != null && ifobtTest.getAnalyseDatum() != null && afspraak.getIngevoerd().after(ifobtTest.getAnalyseDatum()))
							{

								isIfobtUitslagOngunstig = false;
								break;
							}
						}
					}
				}
				return isIfobtUitslagOngunstig && (IFOBTTestStatus.UITGEVOERD.equals(ifobtTest.getStatus()) || IFOBTTestStatus.DOETNIETMEE.equals(ifobtTest.getStatus()));
			}
		}
		return false;
	}

	@Override
	public boolean heeftOnafgerondeVerwijzingOmMedischeRedenen(Afspraak afspraak)
	{
		afspraak = (Afspraak) HibernateHelper.deproxy(afspraak);

		if (afspraak instanceof ColonIntakeAfspraak)
		{
			ColonIntakeAfspraak intakeAfspraak = (ColonIntakeAfspraak) afspraak;
			ColonConclusie conclusie = intakeAfspraak.getConclusie();
			return conclusie != null
				&& conclusie.getType() == ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM
				&& Boolean.TRUE.equals(conclusie.getDoorverwijzingBevestigd())
				&& intakeAfspraak.getNieuweAfspraak() == null;
		}
		return false;
	}

	@Override
	public List<Object> getAfsprakenKamersInRanges(Kamer location, List<Range<Date>> verwijderdeRanges)
	{
		return afspraakDao.getAfsprakenInRanges(location, verwijderdeRanges);
	}

	@Override
	public RoosterItem getRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		return afspraakDao.getRoosterBlokVoorAfspraak(newAfspraak);
	}

	@Override
	public List<Object> getRoosterItemsBezetMetAfspraak(Long roosterItemId, Range<Date> currentViewRanges)
	{
		return afspraakDao.getRoosterItemsBezetMetAfspraak(roosterItemId, currentViewRanges);
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenInRange(Range<Date> range)
	{
		return afspraakDao.getAfsprakenInRange(range);
	}

	@Override
	public RoosterItem getVrijRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		return afspraakDao.getVrijRoosterBlokVoorAfspraak(newAfspraak);
	}

	@Override
	public boolean isDoorverwezenOmMedischeRedenenZonderNieuweAfspraak(Client client)
	{
		ColonIntakeAfspraak laatsteAfspraak = client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			return heeftOnafgerondeVerwijzingOmMedischeRedenen(laatsteAfspraak);
		}
		return false;
	}

	@Override
	public boolean isAfspraakVerwezenOmMedischeRedenen(ColonIntakeAfspraak afspraak)
	{
		boolean isVerwezen = isAfspraakMedischeVerwijzing(afspraak) && afspraak.getNieuweAfspraak() != null;
		var oudeAfspraak = zoekBevestigdeDoorverwijzendeAfspraak(afspraak);
		if (oudeAfspraak != null)
		{
			isVerwezen = true;
		}
		return isVerwezen;
	}

	@Override
	public ColonIntakeAfspraak zoekBevestigdeDoorverwijzendeAfspraak(ColonIntakeAfspraak afspraak)
	{
		var oudeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(afspraak.getOudeAfspraak());
		while (oudeAfspraak != null)
		{
			if (isAfspraakMedischeVerwijzing(oudeAfspraak))
			{
				return oudeAfspraak;
			}
			oudeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(oudeAfspraak.getOudeAfspraak());
		}
		return null;
	}

	private boolean isAfspraakMedischeVerwijzing(ColonIntakeAfspraak afspraak)
	{
		var conclusie = afspraak.getConclusie();
		return conclusie != null && ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(conclusie.getType()) && Boolean.TRUE.equals(
			conclusie.getDoorverwijzingBevestigd());
	}
}
