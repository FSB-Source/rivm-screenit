package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import nl.rivm.screenit.mamma.se.dto.actions.AfrondenDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekAfrondenDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieAfbeeldingSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.SignalerenSeDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.OnderzoekAfrondenService;
import nl.rivm.screenit.mamma.se.service.dtomapper.AfbeeldingDtoMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaAnnotatieIcoon;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaSignaleren;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAnnotatieAfbeeldingService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class OnderzoekAfrondenServiceImpl implements OnderzoekAfrondenService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaBaseAnnotatieAfbeeldingService baseAnnotatieAfbeeldingService;

	@Autowired
	private BerichtToBatchService hl7BerichtenToBatchService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Override
	public void beeindigen(AfrondenDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		beeindigAfspraak(afspraak);
		SignalerenSeDto signalerenSeDto = action.getSignaleren();
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		beeindigOnderzoek(onderzoek, instellingGebruiker, signalerenSeDto, transactieDatumTijd);
		hl7BerichtenToBatchService.queueMammaHL7v24BerichtUitgaand(afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient(), MammaHL7v24ORMBerichtStatus.COMPLETED);
		baseKansberekeningService.kansberekeningHerzien(afspraak.getUitnodiging().getScreeningRonde().getDossier(), transactieDatumTijd.toLocalDate());
	}

	private void beeindigAfspraak(MammaAfspraak afspraak)
	{
		afspraak.setStatus(MammaAfspraakStatus.BEEINDIGD);
		hibernateService.saveOrUpdate(afspraak);
	}

	private void beeindigOnderzoek(MammaOnderzoek onderzoek, InstellingGebruiker instellingGebruiker, SignalerenSeDto signalering, LocalDateTime transactieDatumTijd)
	{
		maakSignalering(instellingGebruiker, onderzoek, signalering, transactieDatumTijd);
	}

	@Override
	public void onderzoekAfronden(OnderzoekAfrondenDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		onderzoek.setAfgerondOp(DateUtil.toUtilDate(transactieDatumTijd));
	}

	@Override
	public void maakSignalering(InstellingGebruiker instellingGebruiker, MammaOnderzoek onderzoek, SignalerenSeDto signaleringDto, LocalDateTime transactieDatumTijd)
	{
		MammaSignaleren signalering = onderzoek.getSignaleren();
		if (onderzoek.getSignaleren() == null)
		{
			signalering = new MammaSignaleren();
			signalering.setOnderzoek(onderzoek);
			onderzoek.setSignaleren(signalering);
		}
		signalering.setHeeftAfwijkingen(signaleringDto.getHeeftAfwijkingen());
		if (onderzoek.getAfspraak().getStatus() == MammaAfspraakStatus.BEEINDIGD)
		{
			afrondenSignalering(instellingGebruiker, onderzoek, transactieDatumTijd);
		}

		if (signaleringDto.getDoorsnedeAfbeeldingen() != null)
		{
			signalering.setRechtsVerticaleDoorsnede(
				maakDoorsnede(signaleringDto.getDoorsnedeAfbeeldingen().getRechtsVerticaleDoorsnede(), signalering.getRechtsVerticaleDoorsnede()));
			signalering.setLinksVerticaleDoorsnede(
				maakDoorsnede(signaleringDto.getDoorsnedeAfbeeldingen().getLinksVerticaleDoorsnede(), signalering.getLinksVerticaleDoorsnede()));
			signalering.setRechtsHorizontaleDoorsnede(
				maakDoorsnede(signaleringDto.getDoorsnedeAfbeeldingen().getRechtsHorizontaleDoorsnede(), signalering.getRechtsHorizontaleDoorsnede()));
			signalering.setLinksHorizontaleDoorsnede(
				maakDoorsnede(signaleringDto.getDoorsnedeAfbeeldingen().getLinksHorizontaleDoorsnede(), signalering.getLinksHorizontaleDoorsnede()));
		}
		hibernateService.saveOrUpdate(signalering);
		hibernateService.saveOrUpdate(onderzoek);
	}

	private void afrondenSignalering(InstellingGebruiker instellingGebruiker, MammaOnderzoek onderzoek, LocalDateTime transactieDatumTijd)
	{
		onderzoek.getSignaleren().setAfgerondDoor(instellingGebruiker);
		onderzoek.getSignaleren().setAfgerondOp(DateUtil.toUtilDate(transactieDatumTijd));
		hibernateService.saveOrUpdate(onderzoek.getSignaleren());
	}

	private MammaAnnotatieAfbeelding maakDoorsnede(AnnotatieAfbeeldingSeDto doorsnede, MammaAnnotatieAfbeelding huidigeAfbeelding)
	{
		if (doorsnede != null && doorsnede.getIconen() != null && !doorsnede.getIconen().isEmpty())
		{
			List<MammaAnnotatieIcoon> doorsnedeIconen = doorsnede.getIconen().stream().map(new AfbeeldingDtoMapper()::icoonDtoToAnnotatieIcoon).collect(Collectors.toList());
			MammaAnnotatieAfbeelding afbeelding = huidigeAfbeelding != null ? huidigeAfbeelding : new MammaAnnotatieAfbeelding();
			baseAnnotatieAfbeeldingService.updateIconenInAfbeelding(doorsnedeIconen, afbeelding);
			return afbeelding;
		}
		return null;
	}
}
