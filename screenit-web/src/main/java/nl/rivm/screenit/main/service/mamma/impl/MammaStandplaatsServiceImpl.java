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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.impl.PersoonCoordinaten;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;
import com.google.common.collect.RangeSet;
import com.google.common.collect.TreeRangeSet;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaStandplaatsServiceImpl implements MammaStandplaatsService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseStandplaatsDao standplaatsDao;

	@Autowired
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Autowired
	private MammaBaseAfspraakDao baseAfspraakDao;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private CoordinatenService coordinatenService;

	private static final ExecutorService EXECUTOR_SERVICE = Executors.newSingleThreadExecutor();

	@Override
	public boolean saveOrUpdateStandplaats(MammaStandplaats standplaats, InstellingGebruiker ingelogdeGebruiker)
	{
		if (standplaats.getLocatie() == null)
		{
			MammaStandplaatsLocatie locatie = new MammaStandplaatsLocatie();
			locatie.setToonHuisnummerInBrieven(true);
			standplaats.setLocatie(locatie);
			MammaStandplaatsLocatie tijdelijkeLocatie = new MammaStandplaatsLocatie();
			tijdelijkeLocatie.setToonHuisnummerInBrieven(true);
			standplaats.setTijdelijkeLocatie(tijdelijkeLocatie);
			tijdelijkeLocatie.setTijdelijk(true);
			hibernateService.saveOrUpdateAll(locatie, tijdelijkeLocatie);
		}

		if (!standplaats.getActief())
		{
			List<MammaStandplaatsRonde> teVerwijderenStandplaatsRonden = new ArrayList<>();
			for (MammaStandplaatsRonde ronde : standplaats.getStandplaatsRonden())
			{
				boolean heeftScreeningRondenOfAfspraken = standplaatsDao.heeftStandplaatsRondenBijScreeningsRonden(ronde) || standplaatsDao.heeftAfspraken(ronde);
				if (!heeftScreeningRondenOfAfspraken)
				{
					teVerwijderenStandplaatsRonden.add(ronde);
					hibernateService.deleteAll(ronde.getStandplaatsPerioden());
				}
			}
			hibernateService.deleteAll(teVerwijderenStandplaatsRonden);
		}
		String melding = "";
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(standplaats, hibernateService.getHibernateSession());

		boolean isNieuw = standplaats.getId() == null;
		if (isNieuw)
		{
			melding += "Standplaats '" + standplaats.getNaam() + "' aangemaakt.";
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += "Standplaats '" + standplaats.getNaam() + "' gewijzigd (" + diffToLatestVersion + ").";
		}
		if (StringUtils.isNotBlank(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_STANDPLAATS, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdate(standplaats);
			baseConceptPlanningsApplicatie.sendStandplaats(standplaats, isNieuw);
			return true;
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaats> zoekStandplaatsen(MammaStandplaats zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		return standplaatsDao.zoekStandplaatsen(zoekObject, first, count, sortProperty, asc);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public long countStandplaatsen(MammaStandplaats zoekObject)
	{
		return standplaatsDao.countStandplaatsen(zoekObject);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public long countActieveStandplaatsPeriodes(MammaStandplaats standplaats)
	{
		return standplaatsDao.countActieveStandplaatsPeriodes(standplaats);
	}

	@Override
	public boolean saveOrUpdateStandplaatsOpmerking(MammaStandplaatsOpmerking opmerking, MammaStandplaats standplaats, InstellingGebruiker loggedInInstellingGebruiker)
	{
		opmerking.setCreatieDatum(dateSupplier.getDate());
		if (opmerking.getId() == null && standplaats != null)
		{
			opmerking.setStandplaats(standplaats);
			standplaats.getStandplaatsOpmerkingen().add(opmerking);
		}
		if (standplaats == null)
		{
			standplaats = opmerking.getStandplaats();
		}

		String melding = "";
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(opmerking, hibernateService.getHibernateSession());

		if (opmerking.getId() == null)
		{
			melding += "Opmerking voor standplaats '" + standplaats.getNaam() + "' aangemaakt.";
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += "Opmerking voor standplaats '" + standplaats.getNaam() + "' gewijzigd.";
		}
		if (StringUtils.isNotBlank(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_STANDPLAATS, loggedInInstellingGebruiker, melding, Bevolkingsonderzoek.MAMMA);

			hibernateService.saveOrUpdateAll(opmerking, standplaats);
			return true;
		}
		return false;
	}

	@Override
	public boolean saveOrUpdateStandplaatsLocatie(MammaStandplaatsLocatie locatie, UploadDocument nieuweBijlage, MammaStandplaats standplaats,
		InstellingGebruiker ingelogdeGebruiker, String oudeAdres, Range<Date> oudePeriode)
	{
		if (nieuweBijlage != null)
		{
			UploadDocument oudeBijlage = locatie.getStandplaatsLocatieBijlage();
			if (oudeBijlage != null && !oudeBijlage.equals(nieuweBijlage))
			{
				oudeBijlage.setActief(false);
				hibernateService.saveOrUpdate(oudeBijlage);
			}
			locatie.setStandplaatsLocatieBijlage(nieuweBijlage);
			try
			{
				uploadDocumentService.saveOrUpdate(nieuweBijlage, FileStoreLocation.MAMMA_STANDPLAATS_LOCATIE_BIJLAGE, standplaats.getId(), true);
			}
			catch (IllegalStateException | IOException e)
			{
				LOG.error("Fout bij opslaan locatie bijlage", e);
			}
		}
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(locatie, hibernateService.getHibernateSession());
		String melding = "";

		if (diffToLatestVersion.length() > 0)
		{
			if (standplaats.getLocatie().equals(locatie))
			{
				melding += "Locatie";
			}
			else
			{
				melding += "Tijdelijke locatie";
			}
			melding += " voor standplaats '" + standplaats.getNaam() + "' gewijzigd.";
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_STANDPLAATS, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdate(locatie);
			maakBrievenVoorGewijzigdeLocatieGegevens(standplaats, locatie, oudeAdres, oudePeriode);
			return true;
		}
		return false;
	}

	private void maakBrievenVoorGewijzigdeLocatieGegevens(MammaStandplaats standplaats, MammaStandplaatsLocatie locatie, String oudeAdres, Range<Date> oudePeriode)
	{
		if (locatie.getTijdelijk())
		{
			Range<Date> nieuwePeriode = Range.closed(DateUtil.startDag(locatie.getStartDatum()), DateUtil.eindDag(locatie.getEindDatum()));
			boolean adresGewijzigd = !AdresUtil.getVolledigeAdresString(locatie).equals(oudeAdres);
			Set<MammaAfspraak> afsprakenInGewijzigdePeriodes = zoekAfsprakenInGewijzigdePeriode(standplaats, oudePeriode, nieuwePeriode, adresGewijzigd);

			if (!afsprakenInGewijzigdePeriodes.isEmpty())
			{
				EXECUTOR_SERVICE
					.submit(new GewijzigdeLocatieBrievenThread(standplaats.getId(),
						afsprakenInGewijzigdePeriodes.stream().map(AbstractHibernateObject::getId).collect(Collectors.toList())));
			}
		}
	}

	Set<MammaAfspraak> zoekAfsprakenInGewijzigdePeriode(MammaStandplaats standplaats, Range<Date> oudePeriode, Range<Date> nieuwePeriode, boolean adresGewijzigd)
	{
		RangeSet<Date> periodesVoorZoeken = TreeRangeSet.create();
		periodesVoorZoeken.add(nieuwePeriode);

		if (oudePeriode != null)
		{
			periodesVoorZoeken.add(oudePeriode);

			if (!adresGewijzigd && oudePeriode.isConnected(nieuwePeriode))
			{
				periodesVoorZoeken.remove(oudePeriode.intersection(nieuwePeriode));
			}
		}

		Date brievenGenererenVanaf = DateUtil.startDag(DateUtil.toUtilDate(dateSupplier.getLocalDate().plusDays(1)));
		if (!periodesVoorZoeken.isEmpty() && periodesVoorZoeken.span().lowerEndpoint().before(brievenGenererenVanaf))
		{
			periodesVoorZoeken.remove(Range.closed(periodesVoorZoeken.span().lowerEndpoint(), brievenGenererenVanaf));
		}

		Set<MammaAfspraak> afspraken = new HashSet<>();
		periodesVoorZoeken.asRanges().forEach(range -> afspraken.addAll(baseAfspraakDao.getAfspraken(standplaats, range, MammaAfspraakStatus.GEPLAND)));
		return afspraken;
	}

	private class GewijzigdeLocatieBrievenThread extends OpenHibernate5SessionInThread
	{

		private final Long standplaatsId;

		private final List<Long> afsprakenIds;

		GewijzigdeLocatieBrievenThread(Long standplaatsId, List<Long> afsprakenIds)
		{
			super(true);
			this.standplaatsId = standplaatsId;
			this.afsprakenIds = afsprakenIds;
		}

		@Override
		protected void runInternal()
		{
			List<MammaBrief> brieven = new ArrayList<>();
			MammaStandplaats persistentStandplaats = hibernateService.get(MammaStandplaats.class, standplaatsId);
			for (Long afspraakId : afsprakenIds)
			{
				MammaAfspraak afspraak = hibernateService.get(MammaAfspraak.class, afspraakId);

				if (afspraak.equals(MammaScreeningRondeUtil.getLaatsteAfspraak(afspraak.getUitnodiging().getScreeningRonde())))
				{
					brieven.add(baseBriefService.maakBvoBrief(afspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET));
				}
			}
			baseStandplaatsService.zetBrievenKlaarVoorStandplaatsVoorAfdrukken(brieven, persistentStandplaats);
		}

	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String magStandplaatsInactiveren(MammaStandplaats standplaats)
	{
		if (!standplaats.getPostcodeReeksen().isEmpty())
		{
			return "inactiveren.title.postcodereeks";
		}
		if (!standplaatsDao.magStandplaatsInactiveren(standplaats, dateSupplier.getDateMidnight()))
		{
			return "inactiveren.bevat.nog.afspraken.of.uitnodiging";
		}

		return "";
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaStandplaats getStandplaatsMetPostcode(Client client)
	{
		String postcode = SpringBeanProvider.getInstance().getBean(ClientService.class).getGbaPostcode(client);
		if (StringUtils.isNotBlank(postcode))
		{
			return standplaatsDao.getStandplaatsMetPostcode(postcode);
		}
		return null;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String controleerUitnodigingenNaVeranderingLocatie(MammaStandplaats standplaats)
	{
		MammaStandplaatsLocatie tijdelijkAdres = standplaats.getTijdelijkeLocatie();
		long aantalAfsprakenBinnenLocatie = 0;
		if (tijdelijkAdres.getStartDatum() != null)
		{
			aantalAfsprakenBinnenLocatie += baseAfspraakDao.countAfspraken(standplaats, standplaats.getLocatie().getStartDatum(), tijdelijkAdres.getStartDatum(),
				MammaAfspraakStatus.GEPLAND);
			aantalAfsprakenBinnenLocatie += baseAfspraakDao.countAfspraken(standplaats,
				DateUtil.toUtilDate(DateUtil.toLocalDate(tijdelijkAdres.getEindDatum()).plusDays(1)), null,
				MammaAfspraakStatus.GEPLAND);
		}

		if (aantalAfsprakenBinnenLocatie > 0)
		{
			return "zijn.al.uitnodigingen.nieuwe.locatie";
		}
		return "";
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String controleerUitnodigingenNaVeranderingTijdelijkeLocatie(MammaStandplaats standplaats, String oudeAdres, Range<Date> oudePeriode)
	{
		MammaStandplaatsLocatie locatie = standplaats.getTijdelijkeLocatie();
		Date nieuweStartDatum = locatie.getStartDatum() != null ? DateUtil.startDag(locatie.getStartDatum()) : null;
		Date nieuweEindDatum = locatie.getEindDatum() != null ? DateUtil.eindDag(locatie.getEindDatum()) : null;

		if (oudePeriode != null)
		{
			if (!DateUtil.compareEquals(nieuweStartDatum, oudePeriode.lowerEndpoint()) || !DateUtil.compareEquals(nieuweEindDatum, oudePeriode.upperEndpoint()))
			{
				long aantalAfsprakenBinnenOudeLocatie = baseAfspraakDao.countAfspraken(standplaats, oudePeriode.lowerEndpoint(), oudePeriode.upperEndpoint(),
					MammaAfspraakStatus.GEPLAND);
				long aantalAfsprakenBinnenNieuweLocatie = baseAfspraakDao.countAfspraken(standplaats, nieuweStartDatum, nieuweEindDatum, MammaAfspraakStatus.GEPLAND);

				if (aantalAfsprakenBinnenOudeLocatie > 0 || aantalAfsprakenBinnenNieuweLocatie > 0)
				{
					if (aantalAfsprakenBinnenOudeLocatie >= aantalAfsprakenBinnenNieuweLocatie)
					{
						return "zijn.al.uitnodigingen.oorspronkelijke.locatie";
					}
					else
					{
						return "zijn.al.uitnodigingen.nieuwe.locatie.tijdelijk";
					}
				}
			}
			if (!AdresUtil.getVolledigeAdresString(locatie).equals(oudeAdres))
			{
				if (baseAfspraakDao.countAfspraken(standplaats, oudePeriode.lowerEndpoint(), oudePeriode.upperEndpoint(), MammaAfspraakStatus.GEPLAND) > 0)
				{
					return "zijn.al.uitnodigingen.locatie.change";
				}
			}
		}
		else
		{
			if (baseAfspraakDao.countAfspraken(standplaats, nieuweStartDatum, nieuweEindDatum, MammaAfspraakStatus.GEPLAND) > 0)
			{
				return "zijn.al.uitnodigingen.locatie.change";
			}
		}
		return "";
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public Double bepaalAfstand(MammaStandplaats standplaats, Client client)
	{
		PostcodeCoordinaten standplaatsPostcodeCoordinaten = standplaats.getTijdelijkeLocatie().getStartDatum() != null
			? standplaats.getTijdelijkeLocatie().getPostcodeCoordinaten()
			: standplaats.getLocatie().getPostcodeCoordinaten();

		PersoonCoordinaten persoonCoordinaten = coordinatenService.getCoordinatenVanPersoon(client.getPersoon());

		if (persoonCoordinaten.vanAdres != null && standplaatsPostcodeCoordinaten != null)
		{
			return BigDecimalUtil.berekenDistance(persoonCoordinaten.vanAdres, standplaatsPostcodeCoordinaten);
		}
		else
		{
			return null;
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaStandplaatsLocatie getStandplaatsLocatie(MammaStandplaats standplaats, Date datum)
	{
		MammaStandplaatsLocatie locatie;
		locatie = standplaats.getLocatie();
		if (standplaats.getTijdelijkeLocatie() != null)
		{
			MammaStandplaatsLocatie tijdelijkeLocatie = standplaats.getTijdelijkeLocatie();
			if ((tijdelijkeLocatie.getStartDatum() != null || tijdelijkeLocatie.getEindDatum() != null) &&
				(tijdelijkeLocatie.getStartDatum() == null || !DateUtil.compareAfter(tijdelijkeLocatie.getStartDatum(), datum))
				&& (tijdelijkeLocatie.getEindDatum() == null || !DateUtil.compareBefore(tijdelijkeLocatie.getEindDatum(), datum)))
			{
				locatie = tijdelijkeLocatie;
			}
		}
		return locatie;
	}

}
