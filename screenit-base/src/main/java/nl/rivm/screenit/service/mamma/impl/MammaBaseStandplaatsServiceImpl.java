package nl.rivm.screenit.service.mamma.impl;

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
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.repository.mamma.MammaStandplaatsRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.impl.IBrievenGeneratorHelper;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsPeriodeService;
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
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification.filterOpActief;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification.filterOpRegio;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification.heeftPostcode;

@Service
public class MammaBaseStandplaatsServiceImpl implements MammaBaseStandplaatsService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseStandplaatsServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

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

	@Autowired
	private MammaStandplaatsRepository standplaatsRepository;

	@Autowired
	private MammaBaseStandplaatsPeriodeService sandplaatsPeriodeService;

	@Override
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter)
	{
		return getStandplaatsPeriodeMetAfstandDtos(client, filter, false);
	}

	@Override
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(Client client, IMammaAfspraakWijzigenFilter filter,
		boolean validatieUitvoeren)
	{
		List<MammaStandplaatsPeriodeMetAfstandDto> standplaatsPeriodeMetAfstandDtos = new ArrayList<>();

		if (filter.getTotEnMet() != null)
		{
			Map<Long, Double> afstandenPerStandplaats = new HashMap<>();
			var standplaatsPerioden = sandplaatsPeriodeService.getStandplaatsPerioden(filter);
			var gefilterdeStandplaatsPerioden = standplaatsPerioden.stream()
				.filter(standplaats -> !validatieUitvoeren || uitstelService.valideerStandplaatsPeriode(standplaats, filter.getVanaf()) == null)
				.collect(Collectors.toList());
			for (var standplaatsPeriode : gefilterdeStandplaatsPerioden)
			{
				var standplaatsId = standplaatsPeriode.getStandplaatsRonde().getStandplaats().getId();
				var afstand = afstandenPerStandplaats.get(standplaatsId);
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
	public List<String> getStandplaatsPlaatsenVanActievePeriodes(IMammaAfspraakWijzigenFilter filter, boolean uitstellen)
	{
		Set<String> plaatsen = new HashSet<>();
		var standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(filter, uitstellen);
		for (var standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			var standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
				standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());

			var vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if ((vrijgegevenTotEnMetDatum != null && !DateUtil.toLocalDate(standplaatsPeriode.getVanaf()).isAfter(vrijgegevenTotEnMetDatum)) || uitstellen)
			{
				var standplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
				plaatsen.add(standplaats.getLocatie().getPlaats());
			}
		}
		return plaatsen.stream().filter(StringUtils::isNotBlank).sorted().collect(Collectors.toList());
	}

	@Override
	public List<MammaStandplaatsPeriodeMetAfstandDto> getStandplaatsPeriodeMetAfstandDtos(IMammaAfspraakWijzigenFilter filter, boolean uitstellen)
	{
		var client = filter.getClient();

		var savedPlaats = filter.getPlaats();
		filter.setPlaats(null);
		if (uitstellen)
		{
			filter.setTotEnMet(filter.getVanaf());
		}
		var standplaatsPeriodeMetAfstandDtos = getStandplaatsPeriodeMetAfstandDtos(client, filter);
		filter.setPlaats(savedPlaats);
		return standplaatsPeriodeMetAfstandDtos;
	}

	@Override
	public MammaStandplaats getStandplaatsMetPostcode(Client client)
	{
		var postcode = clientService.getGbaPostcode(client);
		if (StringUtils.isNotBlank(postcode))
		{
			var standplaats = standplaatsRepository.findOne(heeftPostcode(postcode));
			return standplaats.orElse(null);
		}
		return null;
	}

	@Override
	public MammaStandplaatsPeriode getEerstvolgendeStandplaatsPeriode(MammaStandplaats standplaats)
	{
		var vandaag = dateSupplier.getDateMidnight();
		MammaStandplaatsPeriode eerstVolgendeStandplaatsPeriode = null;
		if (standplaats != null)
		{
			for (var ronde : standplaats.getStandplaatsRonden())
			{
				for (var periode : ronde.getStandplaatsPerioden())
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
	public Double bepaalAfstand(MammaStandplaats standplaats, Client client)
	{
		var standplaatsPostcodeCoordinaten = standplaats.getTijdelijkeLocatie().getStartDatum() != null
			? standplaats.getTijdelijkeLocatie().getPostcodeCoordinaten()
			: standplaats.getLocatie().getPostcodeCoordinaten();

		var persoonCoordinaten = coordinatenService.getCoordinatenVanPersoon(client.getPersoon());

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
	public MammaStandplaatsLocatie getStandplaatsLocatie(MammaStandplaats standplaats, Date datum)
	{
		MammaStandplaatsLocatie locatie;
		locatie = standplaats.getLocatie();
		if (standplaats.getTijdelijkeLocatie() != null)
		{
			var tijdelijkeLocatie = standplaats.getTijdelijkeLocatie();
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
	@Transactional
	public void zetBrievenKlaarVoorStandplaatsVoorAfdrukken(List<MammaBrief> brieven, MammaStandplaats standplaats)
	{
		if (!brieven.isEmpty())
		{
			var nu = dateSupplier.getDate();

			var mergedBrieven = new MammaMergedBrieven();
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

	@Override
	public boolean isActieveStandplaatsPeriodeVerkort(MammaStandplaatsPeriode persistentStandplaatsPeriode, LocalDate nieuweTotEnMet)
	{
		var totEnMet = DateUtil.toLocalDate(persistentStandplaatsPeriode.getTotEnMet());
		var vanaf = DateUtil.toLocalDate(persistentStandplaatsPeriode.getVanaf());
		return nieuweTotEnMet.isBefore(totEnMet) && Range.closed(vanaf, totEnMet)
			.contains(dateSupplier.getLocalDate());
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
			var naam = "";
			var sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
			if (brieven.getCreatieDatum() != null)
			{
				naam += sdf.format(brieven.getCreatieDatum()) + "-";
			}
			if (brieven.getScreeningOrganisatie() != null)
			{
				var soNaam = brieven.getScreeningOrganisatie().getNaam();
				soNaam = soNaam.replace(" ", "_");
				naam += soNaam + "-";
			}
			naam += standplaats.getNaam().replace(" ", "_") + "-";
			if (brieven.getBriefType() != null)
			{
				naam += brieven.getBriefType().name().toLowerCase();
			}
			return naam + ".pdf";
		}

		@Override
		public void verhoogAantalBrievenVanScreeningOrganisatie(MammaMergedBrieven mergedBrieven)
		{
		}

		@Override
		public void additionalMergedContext(MailMergeContext context)
		{
			var ce = clientService.bepaalCe(context.getClient());
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
	public List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie voorRegio)
	{
		return standplaatsRepository.findAll(filterOpActief(true)
				.and(filterOpRegio(voorRegio)),
			Sort.by(Sort.Direction.ASC, MammaStandplaats_.NAAM));
	}

	@Override
	public Date getMaximaleVrijgegevenTotEnMetDatum(List<MammaStandplaatsPeriode> standplaatsPerioden)
	{
		return standplaatsPerioden.stream()
			.filter(standplaatsPeriode -> standplaatsPeriode != null && standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet() != null)
			.max(Comparator.comparing(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet()))
			.map(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet())
			.orElse(null);
	}

	@Override
	public MammaStandplaatsPeriode huidigeStandplaatsPeriodeInRouteVanStandplaats(MammaStandplaats standplaats)
	{
		var eerstvolgendePeriodeStandplaats = getEerstvolgendeStandplaatsPeriode(standplaats);
		if (eerstvolgendePeriodeStandplaats == null)
		{
			return null;
		}

		var vandaag = dateSupplier.getLocalDate();
		var screeningsEenheid = eerstvolgendePeriodeStandplaats.getScreeningsEenheid();
		var gefilterdeStandplaatsPeriodes = screeningsEenheid.getStandplaatsPerioden().stream()
			.filter(p -> p.getTotEnMet().after(DateUtil.toUtilDate(vandaag)) || DateUtil.compareEquals(p.getTotEnMet(), DateUtil.toUtilDate(vandaag)))
			.collect(Collectors.toList());

		for (var periode : gefilterdeStandplaatsPeriodes)
		{
			if (DateUtil.isWithinRange(DateUtil.toLocalDate(periode.getVanaf()), DateUtil.toLocalDate(periode.getTotEnMet()), vandaag))
			{
				return periode;
			}
		}

		return null;
	}
}
