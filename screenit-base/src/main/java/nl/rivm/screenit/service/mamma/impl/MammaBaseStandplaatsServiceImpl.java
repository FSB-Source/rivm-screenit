
package nl.rivm.screenit.service.mamma.impl;

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
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.mamma.MammaBaseAfspraakDao;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.impl.PersoonCoordinaten;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseStandplaatsServiceImpl implements MammaBaseStandplaatsService
{
	
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseStandplaatsServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseStandplaatsDao standplaatsDao;

	@Autowired
	private MammaBaseAfspraakDao baseAfspraakDao;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private CoordinatenService coordinatenService;

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
		InstellingGebruiker ingelogdeGebruiker)
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
				fileService.saveOrUpdateUploadDocument(nieuweBijlage, FileStoreLocation.MAMMA_STANDPLAATS_LOCATIE_BIJLAGE, standplaats.getId(), true);
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
			return true;
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie voorRegio)
	{
		return standplaatsDao.getActieveStandplaatsen(voorRegio);
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
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = new ArrayList<>();

		if (filter.getTotEnMetLocalDate() != null)
		{
			Map<Long, Double> afstandenPerStandplaats = new HashMap<>();
			List<MammaStandplaatsPeriode> standplaatsPerioden = standplaatsDao.getStandplaatsPerioden(filter);
			for (MammaStandplaatsPeriode standplaatsPeriode : standplaatsPerioden)
			{
				Long standplaatsId = standplaatsPeriode.getStandplaatsRonde().getStandplaats().getId();
				Double afstand = afstandenPerStandplaats.get(standplaatsId);
				if (!afstandenPerStandplaats.containsKey(standplaatsId))
				{
					afstand = bepaalAfstand(standplaatsPeriode.getStandplaatsRonde().getStandplaats(), filter.getClient());
					if (afstand != null)
					{
						if (filter.getAfstand() != null && afstand > filter.getAfstand())
						{
							afstand = null;
						}
					}
					else
					{
						afstand = MammaKandidaatAfspraakDto.ONBEKENDE_AFSTAND;
					}
					afstandenPerStandplaats.put(standplaatsId, afstand);
				}
				if (afstand != null)
				{
					standplaatsPeriodeMetAfstandDtos.add(new MammaStandplaatsPeriodeMetAfstandDto(standplaatsPeriode.getId(), afstand));
				}
			}
		}
		return standplaatsPeriodeMetAfstandDtos;
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
	public MammaStandplaatsPeriode getEerstvolgendeStandplaatsPeriode(MammaStandplaats standplaats)
	{
		Date vandaag = dateSupplier.getDateMidnight();
		MammaStandplaatsPeriode eerstVolgendeStandplaatsPeriode = null;
		if (standplaats != null)
		{
			for (MammaStandplaatsRonde ronde : standplaats.getStandplaatsRonden())
			{
				for (MammaStandplaatsPeriode periode : ronde.getStandplaatsPerioden())
				{
					if (!periode.getTotEnMet().before(vandaag)
						&& (eerstVolgendeStandplaatsPeriode == null || eerstVolgendeStandplaatsPeriode.getVanaf().after(periode.getVanaf())))
					{
						eerstVolgendeStandplaatsPeriode = periode;
					}
				}
			}
		}
		return eerstVolgendeStandplaatsPeriode;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String controleerUitnodigingenNaVeranderingLocatie(MammaStandplaats standplaats, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum)
	{
		MammaStandplaatsLocatie tijdelijkAdres = standplaats.getTijdelijkeLocatie();
		long aantalAfsprakenBinnenLocatie = 0;
		if (tijdelijkAdres.getStartDatum() != null)
		{
			aantalAfsprakenBinnenLocatie += baseAfspraakDao.countAfspraken(standplaats, standplaats.getLocatie().getStartDatum(), tijdelijkAdres.getStartDatum(),
				MammaAfspraakStatus.GEPLAND);
			aantalAfsprakenBinnenLocatie += baseAfspraakDao.countAfspraken(standplaats,
				DateUtil.toUtilDateMidnight(DateUtil.toLocalDateTime(tijdelijkAdres.getEindDatum()).plusDays(1)), null,
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
	public String controleerUitnodigingenNaVeranderingTijdelijkeLocatie(MammaStandplaats standplaats, String initieelAdres, Date initieelStartDatum, Date initieelEindDatum)
	{
		MammaStandplaatsLocatie locatie = standplaats.getTijdelijkeLocatie();
		if (initieelStartDatum != null)
		{
			if (!DateUtil.compareEquals(locatie.getStartDatum(), initieelStartDatum) || !DateUtil.compareEquals(locatie.getEindDatum(), initieelEindDatum))
			{
				long initieelAantalAfsprakenBinnenLocatie = baseAfspraakDao.countAfspraken(standplaats, initieelStartDatum,
					DateUtil.toUtilDateMidnight(DateUtil.toLocalDateTime(initieelEindDatum).plusDays(1)), MammaAfspraakStatus.GEPLAND);
				long nieuweAantalAfsprakenBinnenLocatie = baseAfspraakDao.countAfspraken(standplaats, locatie.getStartDatum(),
					DateUtil.toUtilDateMidnight(DateUtil.toLocalDateTime(locatie.getEindDatum()).plusDays(1)), MammaAfspraakStatus.GEPLAND);
				if (initieelAantalAfsprakenBinnenLocatie > nieuweAantalAfsprakenBinnenLocatie)
				{
					return "zijn.al.uitnodigingen.oorspronkelijke.locatie";
				}
				else if (initieelAantalAfsprakenBinnenLocatie < nieuweAantalAfsprakenBinnenLocatie)
				{
					return "zijn.al.uitnodigingen.nieuwe.locatie";
				}
			}
			if (!AdresUtil.getVolledigeAdresString(locatie).equals(initieelAdres))
			{
				if (baseAfspraakDao.countAfspraken(standplaats, initieelStartDatum,
					DateUtil.toUtilDate(DateUtil.toLocalDate(locatie.getEindDatum()).plusDays(1)), MammaAfspraakStatus.GEPLAND) > 0)
				{
					return "zijn.al.uitnodigingen.locatie.change";
				}
			}
		}
		else
		{
			if (baseAfspraakDao.countAfspraken(standplaats, locatie.getStartDatum(),
				DateUtil.toUtilDate(DateUtil.toLocalDate(locatie.getEindDatum()).plusDays(1)), MammaAfspraakStatus.GEPLAND) > 0)
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
