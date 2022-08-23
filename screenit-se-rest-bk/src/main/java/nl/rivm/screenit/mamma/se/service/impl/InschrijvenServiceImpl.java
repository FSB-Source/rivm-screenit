package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.MammaHuisartsDto;
import nl.rivm.screenit.mamma.se.dto.actions.InschrijvenDto;
import nl.rivm.screenit.mamma.se.dto.actions.SetEmailAdresDto;
import nl.rivm.screenit.mamma.se.service.InschrijvenService;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.dtomapper.HuisartsDtoMapper;
import nl.rivm.screenit.mamma.se.service.dtomapper.TijdelijkAdresDtoMapper;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class InschrijvenServiceImpl implements InschrijvenService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BezwaarService bezwaarService;

	@Autowired
	private BriefHerdrukkenService briefHerdrukkenService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Override
	public void inschrijven(InschrijvenDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		baseAfmeldService.heraanmeldenAlsClientAfgemeldIs(afspraak.getUitnodiging().getScreeningRonde().getDossier());
		afspraakWijzigen(action, afspraak, instellingGebruiker);
		afspraakInschrijven(afspraak, instellingGebruiker, transactieDatumTijd);
		opslaanClientgegevens(action, afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient());
		hibernateService.saveOrUpdate(afspraak);
	}

	@Override
	public void inschrijvingWijzigen(InschrijvenDto action, InstellingGebruiker instellingGebruiker)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		baseAfmeldService.heraanmeldenAlsClientAfgemeldIs(afspraak.getUitnodiging().getScreeningRonde().getDossier());
		afspraakWijzigen(action, afspraak, instellingGebruiker);
		opslaanClientgegevens(action, afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient());
		hibernateService.saveOrUpdate(afspraak);
	}

	@Override
	public List<MammaHuisartsDto> getAllHuisartsen()
	{
		Map<String, Boolean> parameters = new HashMap<>();
		parameters.put("verwijderd", Boolean.FALSE);
		List<EnovationHuisarts> enovationHuisartsen = hibernateService.getByParameters(EnovationHuisarts.class, parameters);
		return enovationHuisartsen.stream().map(HuisartsDtoMapper::createMammaHuisarsDto).collect(Collectors.toList());
	}

	@Override
	public void setEmailAdres(SetEmailAdresDto setEmailAdresDto, InstellingGebruiker ingelogdeGebruiker)
	{

	}

	private void huisartsSelecteren(Long enovationHuisartsId, MammaGeenHuisartsOption geenHuisartsOption, MammaScreeningRonde screeningsRonde)
	{
		if (enovationHuisartsId == null && geenHuisartsOption == null)
		{
			throw new IllegalStateException("Huisarts is verplicht bij inschrijven.");
		}
		if (enovationHuisartsId != null)
		{
			screeningsRonde.setGeenHuisartsOptie(null);
			screeningsRonde.setHuisarts(hibernateService.load(EnovationHuisarts.class, enovationHuisartsId));
		}
		else
		{
			screeningsRonde.setHuisarts(null);
			screeningsRonde.setGeenHuisartsOptie(geenHuisartsOption);
		}
		screeningsRonde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());
	}

	private void vraagBezwaarAan(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker)
	{
		Client client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();

		if (Boolean.FALSE.equals(afspraak.getBezwaarAangevraagd()))
		{
			BezwaarBrief bezwaarBrief = bezwaarService.getNogNietVerwerkteBezwaarBrief(client.getBezwaarMomenten());
			if (bezwaarBrief == null)
			{
				BezwaarMoment nieuwBezwaarMoment = new BezwaarMoment();
				nieuwBezwaarMoment.setClient(client);
				nieuwBezwaarMoment.setManier(ClientContactManier.AANVRAAG_FORMULIER);
				bezwaarService.bezwaarAanvragen(client, nieuwBezwaarMoment);
			}
			else
			{
				briefHerdrukkenService.opnieuwAanmaken(bezwaarBrief, instellingGebruiker);
			}
			afspraak.setBezwaarAangevraagd(true);
		}
	}

	private void afspraakWijzigen(InschrijvenDto action, MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker)
	{
		afspraak.setIdentificatiesoort(action.getIdentificatiesoort());
		afspraak.setIdentificatienummer(action.getIdentificatienummer());
		if (action.getBezwaarAangevraagd())
		{
			vraagBezwaarAan(afspraak, instellingGebruiker);
		}
		huisartsSelecteren(action.getHuisartsId(), action.getGeenHuisartsOptie(), afspraak.getUitnodiging().getScreeningRonde());
	}

	private void afspraakInschrijven(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		afspraak.setStatus(MammaAfspraakStatus.INGESCHREVEN);
		afspraak.setIngeschrevenOp(DateUtil.toUtilDate(transactieDatumTijd));
		afspraak.setIngeschrevenDoor(instellingGebruiker);
	}

	private void opslaanClientgegevens(InschrijvenDto inschrijvenDto, Client client)
	{
		GbaPersoon persoon = client.getPersoon();
		if (inschrijvenDto.getTijdelijkAdres() != null)
		{
			opslaanTijdelijkAdres(inschrijvenDto, persoon);
		}
		persoon.setEmailadres(inschrijvenDto.getEmailadres());
		persoon.setTelefoonnummer1(inschrijvenDto.getTelefoonnummer1());
		persoon.setTelefoonnummer2(inschrijvenDto.getTelefoonnummer2());
		hibernateService.saveOrUpdate(persoon);
	}

	private void opslaanTijdelijkAdres(InschrijvenDto inschrijvenDto, GbaPersoon persoon)
	{
		TijdelijkAdres tijdelijkAdres = persoon.getTijdelijkAdres();
		if (tijdelijkAdres == null)
		{
			tijdelijkAdres = new TijdelijkAdres();
			persoon.setTijdelijkAdres(tijdelijkAdres);
		}
		new TijdelijkAdresDtoMapper().updateTijdelijkAdres(tijdelijkAdres, inschrijvenDto.getTijdelijkAdres());
		hibernateService.saveOrUpdate(tijdelijkAdres);
	}
}
