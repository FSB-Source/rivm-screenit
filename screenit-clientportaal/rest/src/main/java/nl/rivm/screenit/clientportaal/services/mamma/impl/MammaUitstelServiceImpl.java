package nl.rivm.screenit.clientportaal.services.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.exception.NotValidException;
import nl.rivm.screenit.clientportaal.mappers.mamma.MammaStandplaatsPeriodeMapper;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaStandplaatsperiodeOptieDto;
import nl.rivm.screenit.clientportaal.services.mamma.MammaAfspraakService;
import nl.rivm.screenit.clientportaal.services.mamma.MammaUitstelService;
import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaUitstelServiceImpl implements MammaUitstelService
{

	private MammaStandplaatsPeriodeMapper standplaatsPeriodeMapper;

	private HibernateService hibernateService;

	private MammaBaseStandplaatsService standplaatsService;

	private MammaBaseUitstelService uitstelService;

	private MammaAfspraakService afspraakService;

	@Override
	public MammaStandplaatsperiodeOptieDto toStandplaatsPeriodeOptie(MammaStandplaatsPeriodeMetAfstandDto standplaatsPeriodeDto,
		MammaAfspraakWijzigenFilterDto wijzigenFilterDto)
	{
		MammaStandplaatsperiodeOptieDto standplaatsPeriodeOptieDto = standplaatsPeriodeMapper.entityToDto(standplaatsPeriodeDto);
		MammaStandplaatsPeriode standplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class,
			standplaatsPeriodeDto.getStandplaatsPeriodeId());
		MammaStandplaatsLocatie locatie = standplaatsService.getStandplaatsLocatie(standplaatsPeriode.getStandplaatsRonde().getStandplaats(),
			DateUtil.toUtilDate(wijzigenFilterDto.getVanaf()));
		standplaatsPeriodeOptieDto.setAdres(locatie.getAdres());
		standplaatsPeriodeOptieDto.setPostcode(locatie.getPostcode());
		standplaatsPeriodeOptieDto.setPlaats(locatie.getPlaats());
		standplaatsPeriodeOptieDto.setStartPeriode(DateUtil.toLocalDateTime(standplaatsPeriode.getVanaf()));
		standplaatsPeriodeOptieDto.setEindPeriode(DateUtil.toLocalDateTime(standplaatsPeriode.getTotEnMet()));
		return standplaatsPeriodeOptieDto;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakUitstelEnSlaOp(Client client, MammaStandplaatsperiodeOptieDto standplaatsPeriodeDto)
	{
		MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
			standplaatsPeriodeDto.getStandplaatsPeriodeId());

		MammaUitstel mammaUitstel = uitstelService.getOfMaakMammaUitstel(client.getMammaDossier().getLaatsteScreeningRonde(),
			standplaatsPeriode.getStandplaatsRonde().getStandplaats(), DateUtil.toUtilDate(standplaatsPeriodeDto.getFilter().getVanaf()));

		String validatieError = uitstelService.valideerStandplaatsPeriode(standplaatsPeriode, DateUtil.toLocalDate(mammaUitstel.getStreefDatum()));
		if (StringUtils.isNotBlank(validatieError))
		{
			throw new NotValidException(validatieError);
		}
		uitstelService.saveUitstel(mammaUitstel, false, client);
	}

	@Override
	public boolean beschikbareStandplaatsperiodesBevatGekozenStandplaatsperiode(MammaStandplaatsperiodeOptieDto standplaatsPeriodeDto, Client client)
	{
		MammaAfspraakWijzigenFilterDto filter = afspraakService.toAfspraakFilter(standplaatsPeriodeDto.getFilter(), client, false);
		List<MammaStandplaatsPeriodeMetAfstandDto> beschikbareStandplaatsPeriodes = standplaatsService.getStandplaatsPeriodeMetAfstandDtos(client, filter, true);

		return beschikbareStandplaatsPeriodes.stream().anyMatch(standplaatsPeriode -> standplaatsPeriode.getStandplaatsPeriodeId().equals(standplaatsPeriodeDto.getStandplaatsPeriodeId()));
	}

}
