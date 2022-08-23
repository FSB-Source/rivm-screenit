package nl.rivm.screenit.clientportaal.services.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.mappers.mamma.MammaAfspraakZoekFilterMapper;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakOptieDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakZoekFilterDto;
import nl.rivm.screenit.clientportaal.services.mamma.MammaAfspraakService;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.dto.mamma.afspraken.MammaHuidigeAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaAfspraakServiceImpl implements MammaAfspraakService
{
	private final MammaAfspraakZoekFilterMapper afspraakZoekFilterMapper;

	private final HibernateService hibernateService;

	private final MammaBaseFactory baseFactory;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MammaBaseStandplaatsDao standplaatsDao;

	private final MammaBaseStandplaatsService standplaatsService;

	@Override
	public List<LocalDate> getAlleDatumsMetBeschikbareAfspraken(Client client, String plaats, String afstand)
	{
		LocalDate vandaag = currentDateSupplier.getLocalDate();

		MammaAfspraakWijzigenFilterDto filterVoorOphalenKandidaatAfspraken = MammaAfspraakWijzigenFilterDto.filterVoorOphalenAfsprakenBinnenVrijgegevenPeriode(client, plaats, afstand, vandaag,
			getMaximaleVrijgegevenTotEnMetDatumBijPlaats(plaats, afstand));
		List<MammaKandidaatAfspraakDto> kandidaatAfspraken = baseAfspraakService.getKandidaatAfspraken(client,
			filterVoorOphalenKandidaatAfspraken);

		return kandidaatAfspraken.stream().map(MammaKandidaatAfspraakDto::getDatum).sorted().distinct().collect(Collectors.toList());
	}

	private LocalDate getMaximaleVrijgegevenTotEnMetDatumBijPlaats(String plaats, String afstand)
	{
		LocalDate vandaag = currentDateSupplier.getLocalDate();

		MammaAfspraakWijzigenFilterDto filterOphalenStandplaatsPerioden = MammaAfspraakWijzigenFilterDto.filterVoorOphalenStandplaatsenViaPlaatsOfAfstand(plaats, afstand, vandaag,
				vandaag.plusYears(2));
		List<MammaStandplaatsPeriode> standplaatsPerioden = standplaatsDao.getStandplaatsPerioden(filterOphalenStandplaatsPerioden);

		return DateUtil.toLocalDate(standplaatsService.getMaximaleVrijgegevenTotEnMetDatum(standplaatsPerioden));
	}

	@Override
	public MammaAfspraakWijzigenFilterDto toAfspraakFilter(MammaAfspraakZoekFilterDto body, Client client, boolean buitenRegio)
	{
		MammaAfspraakWijzigenFilterDto filter = afspraakZoekFilterMapper.afspraakZoekFilterToAfspraakWijzigenFilterDto(body, client);
		filter.setVerzettenReden(MammaVerzettenReden.CLIENTEN_PORTAAL);
		filter.setBuitenRegio(buitenRegio);
		return filter;
	}

	@Override
	public MammaAfspraak toAfspraak(MammaAfspraakOptieDto kandidaatAfspraakDto, Client client)
	{
		MammaCapaciteitBlok capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, kandidaatAfspraakDto.getCapaciteitBlokId());
		MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
			kandidaatAfspraakDto.getStandplaatsPeriodeId());
		MammaUitnodiging uitnodiging = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();
		Date vanaf = DateUtil.toUtilDate(kandidaatAfspraakDto.getDatumTijd());

		return baseFactory.maakDummyAfspraak(uitnodiging, vanaf, capaciteitBlok, standplaatsPeriode, MammaVerzettenReden.CLIENTEN_PORTAAL);
	}

	@Override
	public MammaAfspraakOptieDto toMammaKandidaatOptie(MammaKandidaatAfspraakDto kandidaatAfspraakDto)
	{
		MammaAfspraakOptieDto mammaAfspraakOptieDto = new MammaAfspraakOptieDto(kandidaatAfspraakDto);
		MammaStandplaatsPeriode standplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class,
			kandidaatAfspraakDto.getStandplaatsPeriodeId());
		MammaStandplaatsLocatie locatie = standplaatsService.getStandplaatsLocatie(standplaatsPeriode.getStandplaatsRonde().getStandplaats(),
			DateUtil.toUtilDate(kandidaatAfspraakDto.getDatum()));

		mammaAfspraakOptieDto.setAdres(AdresUtil.getStraatMetHuisnummerVoorStandplaatsLocatie(locatie, false));
		mammaAfspraakOptieDto.setPostcode(locatie.getPostcode());
		mammaAfspraakOptieDto.setPlaats(locatie.getPlaats());

		boolean briefKanNietMeerVerzondenWorden = baseAfspraakService.briefKanNietMeerVerzondenWorden(DateUtil.toUtilDate(kandidaatAfspraakDto.getDatum()));
		mammaAfspraakOptieDto.setToonBevestigingsBriefOptie(briefKanNietMeerVerzondenWorden);
		mammaAfspraakOptieDto.setBevestigingsBrief(briefKanNietMeerVerzondenWorden);
		return mammaAfspraakOptieDto;
	}

	@Override
	public MammaHuidigeAfspraakDto toHuidigeAfspraakDto(MammaAfspraak huidigeAfspraak)
	{
		String naamStandplaats = huidigeAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam();
		String adresStandplaats = AdresUtil.getAdresVoorStandplaatsLocatie(baseAfspraakService.getMammaStandplaatsLocatieAfspraak(huidigeAfspraak));

		MammaHuidigeAfspraakDto huidigeAfspraakDto = new MammaHuidigeAfspraakDto();
		huidigeAfspraakDto.setWeergaveAfspraakMoment(DateUtil.getWeergaveDatumClientportaal(DateUtil.toLocalDateTime(huidigeAfspraak.getVanaf())));
		huidigeAfspraakDto.setNaamStandplaats(naamStandplaats);
		huidigeAfspraakDto.setAdresStandplaats(adresStandplaats);

		return huidigeAfspraakDto;
	}
}
