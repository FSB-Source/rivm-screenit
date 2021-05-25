package nl.rivm.screenit.service.mamma.impl;

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
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.impl.IBrievenGeneratorHelper;
import nl.rivm.screenit.service.impl.PersoonCoordinaten;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
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
	@Lazy
	private ClientService clientService;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CoordinatenService coordinatenService;

	@Lazy
	@Autowired
	private MammaBaseUitstelService uitstelService;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter)
	{
		return getStandplaatsPeriodeMetAfstandDtos(client, filter, false);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter,
		boolean validatieUitvoeren)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = new ArrayList<>();

		if (filter.getTotEnMet() != null)
		{
			Map<Long, Double> afstandenPerStandplaats = new HashMap<>();
			List<MammaStandplaatsPeriode> standplaatsPerioden = standplaatsDao.getStandplaatsPerioden(filter);
			List<MammaStandplaatsPeriode> gefilterdeStandplaatsPerioden = standplaatsPerioden.stream().filter(standplaats -> !validatieUitvoeren || uitstelService.valideerStandplaatsPeriode(standplaats, filter.getVanaf()) == null).collect(Collectors.toList());
			for (MammaStandplaatsPeriode standplaatsPeriode : gefilterdeStandplaatsPerioden)
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
	public List<String> getStandplaatsPlaatsenVanActivePeriodes(IMammaAfspraakWijzigenFilter filter, boolean verzetten)
	{
		Set<String> plaatsen = new HashSet<>();
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(filter, verzetten);
		for (MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
				standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			LocalDate vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null || !verzetten)
			{
				MammaStandplaats standplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
				plaatsen.add(standplaats.getLocatie().getPlaats());
			}
		}
		List<String> plaatsenList = plaatsen.stream().filter(plaatsnaam -> StringUtils.isNotBlank(plaatsnaam)).collect(Collectors.toList());
		Collections.sort(plaatsenList);
		return plaatsenList;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(IMammaAfspraakWijzigenFilter filter, boolean verzetten)
	{
		Client client = filter.getClient();

		String savedPlaats = filter.getPlaats();
		filter.setPlaats(null);
		if (!verzetten)
		{
			filter.setTotEnMet(filter.getVanaf());
		}
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(client, filter);
		filter.setPlaats(savedPlaats);
		return standplaatsPeriodeMetAfstandDtos;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public MammaStandplaats getStandplaatsMetPostcode(Client client)
	{
		String postcode = clientService.getGbaPostcode(client);
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

	@Override
	public void zetBrievenKlaarVoorStandplaatsVoorAfdrukken(List<MammaBrief> brieven, MammaStandplaats standplaats)
	{
		if (brieven.size() > 0)
		{
			Date nu = dateSupplier.getDate();

			MammaMergedBrieven mergedBrieven = new MammaMergedBrieven();
			mergedBrieven.setScreeningOrganisatie(standplaats.getRegio());
			mergedBrieven.setCreatieDatum(nu);
			mergedBrieven.setBriefType(brieven.get(0).getBriefType());
			mergedBrieven.setActief(false);
			mergedBrieven.setAantalBrieven(0);
			mergedBrieven.setVrijgegeven(true);
			hibernateService.saveOrUpdate(mergedBrieven);

			try
			{
				baseBriefService.createOrAddMergedBrieven(brieven, new AfspraakBrievenGeneratorHelper(mergedBrieven, standplaats));
				baseBriefService.completePdf(mergedBrieven);
			}

			catch (Exception e)
			{
				LOG.error("Onbekende fout bij mergen van standplaats brieven", e);
				throw new IllegalStateException();
			}
		}
	}

	private class AfspraakBrievenGeneratorHelper implements IBrievenGeneratorHelper<MammaBrief, MammaMergedBrieven>
	{
		private final MammaMergedBrieven mergedBrieven;

		private final MammaStandplaats standplaats;

		AfspraakBrievenGeneratorHelper(MammaMergedBrieven mergedBrieven, MammaStandplaats standplaats)
		{
			this.mergedBrieven = mergedBrieven;
			this.standplaats = standplaats;

		}

		@Override
		public String getMergedBrievenNaam(MammaMergedBrieven brieven)
		{
			String naam = "";
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
			if (brieven.getCreatieDatum() != null)
			{
				naam += sdf.format(brieven.getCreatieDatum()) + "-";
			}
			if (brieven.getScreeningOrganisatie() != null)
			{
				String soNaam = brieven.getScreeningOrganisatie().getNaam();
				soNaam = soNaam.replaceAll(" ", "_");
				naam += soNaam + "-";
			}
			naam += standplaats.getNaam().replaceAll(" ", "_") + "-";
			if (brieven.getBriefType() != null)
			{
				naam += brieven.getBriefType().name().toLowerCase();
			}
			String naamPlusExtensie = naam += ".pdf";
			return naamPlusExtensie;
		}

		@Override
		public void verhoogAantalBrievenVanScreeningOrganisatie(MammaMergedBrieven mergedBrieven)
		{
		}

		@Override
		public void additionalMergedContext(MailMergeContext context)
		{
			CentraleEenheid ce = clientService.bepaalCe(context.getClient());
			context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, ce);
			context.putValue(MailMergeContext.CONTEXT_MAMMA_TOON_LOCATIE_WIJZIGING_TEKST, Boolean.TRUE);
		}

		@Override
		public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
		{
			return new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA };
		}

		@Override
		public LogGebeurtenis getMergeProbleemLogGebeurtenis()
		{
			return LogGebeurtenis.MAMMA_BRIEF_MERGE_FOUT;
		}

		@Override
		public LogGebeurtenis getOnvolledigAdresLogGebeurtenis()
		{
			return LogGebeurtenis.MAMMA_ONVOLLEDIG_ADRES;
		}

		@Override
		public FileStoreLocation getFileStoreLocation()
		{
			return FileStoreLocation.MAMMA_MERGED_BRIEVEN;
		}

		@Override
		public IDocument getDocumentDefinitie()
		{
			return baseBriefService.getNieuwsteBriefDefinitie(mergedBrieven.getBriefType());
		}

		@Override
		public MammaMergedBrieven getMergedBrieven()
		{
			return mergedBrieven;
		}

	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie voorRegio)
	{
		return standplaatsDao.getActieveStandplaatsen(voorRegio);
	}

}
