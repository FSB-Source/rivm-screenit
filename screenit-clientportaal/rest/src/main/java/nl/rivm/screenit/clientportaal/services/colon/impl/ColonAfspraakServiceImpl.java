package nl.rivm.screenit.clientportaal.services.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.math.BigDecimal;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.mappers.colon.ColonVrijSlotZonderKamerMapper;
import nl.rivm.screenit.clientportaal.model.colon.ColonVrijSlotZonderKamerDto;
import nl.rivm.screenit.clientportaal.services.colon.ColonAfspraakService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
public class ColonAfspraakServiceImpl implements ColonAfspraakService
{

	private final ColonBaseAfspraakService afspraakService;

	private final ColonVrijSlotZonderKamerMapper colonVrijSlotZonderKamerMapper;

	private final PlanningService planningService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public ColonIntakeAfspraak getHuidigeIntakeAfspraak(Client client)
	{
		ColonScreeningRonde laatsteColonScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
		if (laatsteColonScreeningRonde == null)
		{
			return null;
		}

		return laatsteColonScreeningRonde.getLaatsteAfspraak();
	}

	@Override
	public boolean laatsteAfspraakHeeftDefinitieveIntakeconclusie(Client client)
	{
		var laatsteAfspraak = getHuidigeIntakeAfspraak(client);

		return laatsteAfspraak != null && laatsteAfspraak.getConclusie() != null && ColonConclusieType.getDefinitieveIntakeConclusieTypes()
			.contains(laatsteAfspraak.getConclusie().getType());
	}

	@Override
	public ColonVrijSlotZonderKamerDto vanVrijSlotNaarColonVrijSlot(VrijSlotZonderKamer vrijSlot)
	{
		ColonVrijSlotZonderKamerDto vrijColonSlot = colonVrijSlotZonderKamerMapper.vrijSlotToColonVrijSlotZonderKamerDto(vrijSlot);

		ColonIntakelocatie intakelocatie = hibernateService.load(ColonIntakelocatie.class, vrijColonSlot.getIntakelocatieId());
		vrijColonSlot.setAdres(intakelocatie.getEersteAdres().getAdres());
		vrijColonSlot.setPostcode(intakelocatie.getEersteAdres().getPostcode());
		vrijColonSlot.setZiekenhuis(intakelocatie.getNaam());
		return vrijColonSlot;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void intakeAfspraakAfzeggen(Client client)
	{
		ColonIntakeAfspraak intakeAfspraak = client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfspraak();
		if (intakeAfspraak == null)
		{
			throw new IllegalStateException("Intakeafspraak onbekend");
		}
		else
		{
			afspraakService.annuleerAfspraak(intakeAfspraak, client, ColonAfspraakStatus.GEANNULEERD_CLIENT, false);
		}
	}

	@Override
	public ColonIntakeAfspraak initNieuweAfspraak(ColonIntakeAfspraak oudeAfspraak, VrijSlotZonderKamer gekozenVrijSlotZonderKamer)
	{

		if (gekozenVrijSlotZonderKamer.getIntakelocatieId() != null && (gekozenVrijSlotZonderKamer.getStartTijd() != null))
		{
			var beschikbareKamer = planningService.getBeschikbareKamer(DateUtil.toLocalDateTime(gekozenVrijSlotZonderKamer.getStartTijd()),
				gekozenVrijSlotZonderKamer.getIntakelocatieId());

			if (beschikbareKamer == null)
			{
				throw new IllegalStateException("Iemand anders heeft het gekozen tijdstip in de tussentijd gevuld.");
			}

			ColonIntakeAfspraak nieuweAfspraak = new ColonIntakeAfspraak();
			nieuweAfspraak.setKamer(beschikbareKamer);
			nieuweAfspraak.setBezwaar(false);
			nieuweAfspraak.setGewijzigdOp(currentDateSupplier.getLocalDateTime());
			nieuweAfspraak.setStatus(ColonAfspraakStatus.GEPLAND);
			nieuweAfspraak.setAangemaaktOp(currentDateSupplier.getLocalDateTime());

			if (gekozenVrijSlotZonderKamer.getAfstand() != null)
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(gekozenVrijSlotZonderKamer.getAfstand()));
			}
			else
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(45));
			}

			nieuweAfspraak.setColonScreeningRonde(oudeAfspraak.getColonScreeningRonde());
			nieuweAfspraak.setClient(oudeAfspraak.getClient());
			nieuweAfspraak.setVanaf(DateUtil.toLocalDateTime(gekozenVrijSlotZonderKamer.getStartTijd()));
			nieuweAfspraak.setTot(DateUtil.toLocalDateTime(gekozenVrijSlotZonderKamer.getEindTijd()));

			return nieuweAfspraak;
		}
		return null;
	}
}
