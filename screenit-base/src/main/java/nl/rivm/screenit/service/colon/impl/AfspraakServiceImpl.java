
package nl.rivm.screenit.service.colon.impl;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.colon.AfspraakDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.NieuweIntakeAfspraakMakenReden;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.RedenAfspraakAfzeggen;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.planning.model.IParticipant;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.Location;

import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.SUPPORTS)
@Service
public class AfspraakServiceImpl implements AfspraakService
{

	private static final Logger LOGGER = LoggerFactory.getLogger(AfspraakServiceImpl.class);

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

			return new ArrayList<IAppointment>(afspraakDao.getAfspraken(start, end, filter, null, AfspraakStatus.VOOR_AGENDA));
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
		List<Location> locaties = new ArrayList<Location>();
		locaties.add(locatie);

		return getAppointments(locaties, start, end, showSchedule);
	}

	@Override
	public List<IAppointment> getAppointments(List<Location> locaties, Date start, Date end, boolean showSchedule)
	{
		Afspraak filter = new Afspraak();

		filter.setStatus(AfspraakStatus.GEPLAND);
		List<IAppointment> returnValues = new ArrayList<IAppointment>(afspraakDao.getAfspraken(start, end, filter, locaties, AfspraakStatus.VOOR_AGENDA));

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
	public void annuleerAfspraak(Afspraak afspraak, Account account, AfspraakStatus status, boolean briefTegenhouden)
	{
		DateTime nu = currentDateSupplier.getDateTime();
		Client client = afspraak.getClient();
		String afzegReden = "";
		if (afspraak instanceof ColonIntakeAfspraak)
		{
			ColonIntakeAfspraak colonIntakeAfspraak = (ColonIntakeAfspraak) afspraak;
			afspraakAfzeggen(colonIntakeAfspraak, status, nu, briefTegenhouden);

			ColonScreeningRonde colonScreeningRonde = colonIntakeAfspraak.getColonScreeningRonde();
			if (colonScreeningRonde.getOpenUitnodiging() == null) 

			{
				colonScreeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
				colonScreeningRonde.setAfgerondReden(colonIntakeAfspraak.getRedenAfzeggen().toString());
				colonScreeningRonde.setStatusDatum(nu.toDate());
			}
			else
			{
				OpenUitnodiging uitnodiging = colonScreeningRonde.getOpenUitnodiging();
				uitnodiging.setUitslag(null);
				uitnodiging.setDatum(null);
				uitnodiging.setAfspraak(null);
				hibernateService.saveOrUpdate(uitnodiging);
			}
			RedenAfspraakAfzeggen redenAfzeggen = colonIntakeAfspraak.getRedenAfzeggen();
			if (redenAfzeggen != null)
			{
				afzegReden = " (Reden: " + redenAfzeggen + ")";
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
	public void afspraakAfzeggen(ColonIntakeAfspraak afspraak, AfspraakStatus status, DateTime nu, boolean briefTegenhouden)
	{
		setAfspraakStatus(afspraak, status);
		afspraak.setAfzegDatum(nu.plusMillis(100).toDate());
		hibernateService.saveOrUpdate(afspraak);

		ColonScreeningRonde screeningRonde = afspraak.getColonScreeningRonde();
		dossierBaseService.setDatumVolgendeUitnodiging(screeningRonde.getDossier(), ColonUitnodigingsintervalType.GEANNULEERDE_INTAKE_AFSPRAAK);

		if (!AfspraakStatus.GEANNULEERD_OPEN_UITNODIGING.equals(status))
		{
			Client client = afspraak.getClient();
			if (!client.getPersoon().getGbaAdres().getGbaGemeente().getCode().equals(Gemeente.RNI_CODE))
			{
				ColonBrief colonBrief = briefService.maakColonBrief(screeningRonde, BriefType.COLON_INTAKE_AFMELDING, nu.plusMillis(150).toDate());
				colonBrief.setTegenhouden(briefTegenhouden);
				hibernateService.saveOrUpdate(colonBrief);

				MailMergeContext context = new MailMergeContext();
				context.setClient(client);
				context.setIntakeAfspraak(afspraak);
				if (screeningRonde != null && screeningRonde.getLaatsteUitnodiging() != null)
				{
					context.setColonUitnodiging(screeningRonde.getLaatsteUitnodiging());
				}

				if (!briefTegenhouden)
				{
					berichtenService.verstuurColonHuisartsBericht(client, screeningRonde, HuisartsBerichtType.ANNULEREN_INTAKEAFSPRAAK, context);
				}
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
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
	public Date getLaatsteWijzigingsdatumAfspraak(HibernateObject entity)
	{
		return afspraakDao.getLaatsteWijzigingsdatumAfspraak(entity);
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
	public void verplaatsAfspraak(ColonIntakeAfspraak nieuweAfspraak, Account account, BriefType briefType, boolean briefTegenhouden, boolean binnenRooster)
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
		setAfspraakStatus(laatsteAfspraak, AfspraakStatus.VERPLAATST);

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
		ColonBrief brief = briefService.maakColonBrief(colonScreeningRonde, briefType);
		brief.setIntakeAfspraak(nieuweAfspraak);
		if (briefTegenhouden)
		{
			brief.setTegenhouden(true);
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
			LOGGER.error("Huisarts Bericht kon niet worden aangemaakt. ", e);
		}
	}

	@Override
	public void maakNieuweAfspraak(Client client, NieuweIntakeAfspraakMakenReden reden, ColonIntakeAfspraak nieuweAfspraak, boolean briefTegenhouden, boolean binnenRooster,
		BriefType briefType, Account account)
	{
		OpenUitnodiging openUitnodiging = null;
		ColonDossier colonDossier = client.getColonDossier();
		DateTime nu = currentDateSupplier.getDateTime();
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		nieuweAfspraak.setColonScreeningRonde(laatsteScreeningRonde);
		nieuweAfspraak.setClient(client);
		nieuweAfspraak.setDatumLaatsteWijziging(nu.plusMillis(100).toDate());
		nieuweAfspraak.setNieuweAfspraakMakenReden(reden);
		RoosterItem roosterItem = null;
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
		client.getAfspraken().add(nieuweAfspraak);
		hibernateService.saveOrUpdate(client);
		ColonBrief brief = null;
		if (openUitnodiging != null && !heefAlOpenUitnodigingsBriefGehad)
		{
			brief = briefService.maakColonBrief(laatsteScreeningRonde, BriefType.COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING, nu.plusMillis(150).toDate());
			brief.setIntakeAfspraak(nieuweAfspraak);
		}
		else if (NieuweIntakeAfspraakMakenReden.HANDMATIG_INPLANNEN.equals(reden))
		{
			if (briefType != null)
			{
				brief = briefService.maakColonBrief(laatsteScreeningRonde, briefType, nu.plusMillis(150).toDate());

				if (BriefType.COLON_UITNODIGING_INTAKE.equals(briefType))
				{
					brief.setIfobtTest(ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteScreeningRonde));
				}
			}
			else
			{
				brief = briefService.maakColonBrief(laatsteScreeningRonde, BriefType.COLON_UITNODIGING_INTAKE, nu.plusMillis(150).toDate());
				brief.setIfobtTest(ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteScreeningRonde));
			}
			brief.setIntakeAfspraak(nieuweAfspraak);

		}
		else
		{
			if (briefType != null)
			{
				brief = briefService.maakColonBrief(laatsteScreeningRonde, briefType, nu.plusMillis(150).toDate());
				brief.setIntakeAfspraak(nieuweAfspraak);
			}
			else
			{
				brief = briefService.maakColonBrief(laatsteScreeningRonde, BriefType.COLON_INTAKE_GEWIJZIGD, nu.plusMillis(150).toDate());
				brief.setIntakeAfspraak(nieuweAfspraak);

			}
		}
		if (briefTegenhouden)
		{
			brief.setTegenhouden(true);
		}
		hibernateService.saveOrUpdate(brief);

		laatsteScreeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
		laatsteScreeningRonde.setStatusDatum(nu.plusMillis(200).toDate());
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
			else
			{
				berichtType = HuisartsBerichtType.ONGUNSTIGE_UITSLAG;
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
			berichtenService.verstuurColonHuisartsBericht(client, laatsteScreeningRonde, berichtType, context, true);
		}
		catch (Exception e)
		{
			LOGGER.error("Huisarts Bericht kon niet worden aangemaakt. ", e);
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
		boolean magWijzigenAfzeggen = isLaatsteRonde && !heeftVerslagenInLaatsteRonde && rondeAfspraakIsLopend
			&& (AfspraakStatus.GEPLAND.equals(status) || AfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType));
		return magWijzigenAfzeggen;
	}

	@Override
	public List<Object> getAfsprakenKamersInIntervals(Kamer location, List<Interval> verwijderdeIntervals)
	{
		return afspraakDao.getAfsprakenInIntervals(location, verwijderdeIntervals);
	}

	@Override
	public RoosterItem getRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		return afspraakDao.getRoosterBlokVoorAfspraak(newAfspraak);
	}

	@Override
	public List<Object> getRoosterItemsBezetMetAfspraak(Long roosterItemId, Interval currentViewInterval)
	{
		return afspraakDao.getRoosterItemsBezetMetAfspraak(roosterItemId, currentViewInterval);
	}

	@Override
	public RoosterItem getVrijRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		return afspraakDao.getVrijRoosterBlokVoorAfspraak(newAfspraak);
	}

}
