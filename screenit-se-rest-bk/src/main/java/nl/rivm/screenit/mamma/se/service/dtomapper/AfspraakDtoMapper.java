package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.AdresSeDto;
import nl.rivm.screenit.mamma.se.dto.AfspraakSeDto;
import nl.rivm.screenit.mamma.se.dto.AfspraakStatusSe;
import nl.rivm.screenit.mamma.se.dto.ClientSeDto;
import nl.rivm.screenit.mamma.se.dto.PassantSeDto;
import nl.rivm.screenit.mamma.se.dto.TijdelijkAdresSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.MammografieSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.OnderzoekSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.SignalerenSeDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.organisatie.model.Adres;

public class AfspraakDtoMapper
{

	public AfspraakSeDto createAfspraakSeDto(MammaAfspraak afspraak)
	{
		ClientSeDto clientSeDto = createClientSeDto(afspraak);
		AfspraakSeDto afspraakSeDto = new AfspraakSeDto();
		afspraakSeDto.setId(afspraak.getId());
		afspraakSeDto.setClient(clientSeDto);
		afspraakSeDto.setVanaf(DateUtil.toLocalDateTime(afspraak.getVanaf()));
		afspraakSeDto.setStatus(toAfspraakStatusSe(afspraak.getStatus()));
		afspraakSeDto.setUitnodigingsNr(afspraak.getUitnodiging().getScreeningRonde().getUitnodigingsNr());
		afspraakSeDto.setIdentificatiesoort(afspraak.getIdentificatiesoort());
		afspraakSeDto.setIdentificatienummer(afspraak.getIdentificatienummer());
		afspraakSeDto.setBezwaarAangevraagd(afspraak.getBezwaarAangevraagd());
		afspraakSeDto.setHuidigOnderzoek(createHuidigOnderzoekDto(afspraak));
		afspraakSeDto.setMammografie(createMammografieDto(afspraak));
		afspraakSeDto.setSignaleren(createSignalerenDto(afspraak));
		final EnovationHuisarts huisarts = getHuisarts(afspraak);
		afspraakSeDto.setHuisartsId(huisarts != null ? huisarts.getId() : null);
		afspraakSeDto.setGeenHuisartsOptie(getGeenHuisartsOptie(afspraak));
		afspraakSeDto.setDoorgevoerd(afspraak.getOnderzoek() != null && afspraak.getOnderzoek().isDoorgevoerd());
		afspraakSeDto.setCentralAvailable(
			afspraak.getOnderzoek() != null && afspraak.getOnderzoek().getMammografie() != null
				&& MammaMammografieIlmStatus.beeldenBeschikbaarOfBeschikbaarGeweest(afspraak.getOnderzoek().getMammografie().getIlmStatus()));
		afspraakSeDto.setGeforceerd(afspraak.isGeforceerdeAfspraak());

		return afspraakSeDto;
	}

	public PassantSeDto createPassantDto(MammaDossier mammaDossier)
	{
		MammaUitnodiging uitnodiging = mammaDossier.getLaatsteScreeningRonde().getLaatsteUitnodiging();
		MammaAfspraak afspraak = uitnodiging.getLaatsteAfspraak();

		ClientSeDto clientSeDto = createClientSeDto(mammaDossier);
		LocalDateTime afspraakVanaf = null;
		String afspraakScreeningsEenheid = null;
		LocalDateTime uitnodigingsDatum = null;
		boolean eenmaligeAfmelding = false;

		if (!mammaDossier.getLaatsteScreeningRonde().getAangemeld() && mammaDossier.getAangemeld())
		{
			eenmaligeAfmelding = true;
		}
		else if (afspraak != null)
		{
			afspraakVanaf = DateUtil.toLocalDateTime(afspraak.getVanaf());
			afspraakScreeningsEenheid = afspraak.getStandplaatsPeriode().getScreeningsEenheid().getNaam();
		}
		else
		{
			uitnodigingsDatum = DateUtil.toLocalDateTime(mammaDossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum());
		}

		return new PassantSeDto(clientSeDto, afspraakVanaf, afspraakScreeningsEenheid, uitnodigingsDatum, eenmaligeAfmelding);
	}

	public ClientSeDto createClientSeDto(MammaDossier dossier)
	{
		Client client = dossier.getClient();
		GbaPersoon persoon = client.getPersoon();
		return getClientSeDto(dossier, client, persoon);
	}

	private EnovationHuisarts getHuisarts(MammaAfspraak afspraak)
	{
		MammaScreeningRonde ronde = afspraak.getUitnodiging().getScreeningRonde();
		if (geenHuisartsGekozen(afspraak))
		{
			MammaScreeningRonde vorigeRonde = getVorigeRonde(afspraak);
			if (vorigeRonde != null)
			{
				EnovationHuisarts huisarts = vorigeRonde.getHuisarts();
				if (huisarts != null && !huisarts.isVerwijderd())
				{
					return huisarts;
				}
			}
		}
		return ronde.getHuisarts();
	}

	private MammaGeenHuisartsOption getGeenHuisartsOptie(MammaAfspraak afspraak)
	{
		MammaScreeningRonde ronde = afspraak.getUitnodiging().getScreeningRonde();
		if (geenHuisartsGekozen(afspraak))
		{
			MammaScreeningRonde vorigeRonde = getVorigeRonde(afspraak);
			if (vorigeRonde != null)
			{
				return vorigeRonde.getGeenHuisartsOptie();
			}
		}
		return ronde.getGeenHuisartsOptie();
	}

	private boolean geenHuisartsGekozen(MammaAfspraak afspraak)
	{
		MammaScreeningRonde ronde = afspraak.getUitnodiging().getScreeningRonde();
		return ronde.getHuisarts() == null && ronde.getGeenHuisartsOptie() == null;
	}

	private MammaScreeningRonde getVorigeRonde(MammaAfspraak afspraak)
	{
		List<MammaScreeningRonde> alleRondes = afspraak.getUitnodiging().getScreeningRonde().getDossier().getScreeningRondes()
			.stream().sorted(Comparator.comparing(ScreeningRonde::getCreatieDatum)).collect(Collectors.toList());
		if (alleRondes.size() > 1)
		{
			return alleRondes.get(alleRondes.size() - 2);
		}
		return null;
	}

	private ClientSeDto createClientSeDto(MammaAfspraak afspraak)
	{
		return createClientSeDto(afspraak.getUitnodiging().getScreeningRonde().getDossier());
	}

	private ClientSeDto getClientSeDto(MammaDossier dossier, Client client, GbaPersoon persoon)
	{
		ClientSeDto clientSeDto = new ClientSeDto();
		clientSeDto.setId(client.getId());
		clientSeDto.setVoorletters(NaamUtil.getVoorlettersClient(client));
		clientSeDto.setGeboorteTussenvoegsel(persoon.getTussenvoegsel());
		clientSeDto.setGeboorteAchternaam(persoon.getAchternaam());
		clientSeDto.setAanspreekTussenvoegselEnAchternaam(NaamUtil.getAanspreekTussenvoegselEnAchternaam(client));
		clientSeDto.setBsn(persoon.getBsn());
		clientSeDto.setGeboortedatum(DateUtil.toLocalDate(persoon.getGeboortedatum()));
		clientSeDto.setAdres(createAdresSeDto(persoon.getGbaAdres()));
		clientSeDto.setTijdelijkGbaAdres(createAdresSeDto(persoon.getTijdelijkGbaAdres()));
		clientSeDto.setTijdelijkAdres(createTijdelijkAdresSeDto(persoon.getTijdelijkAdres()));
		clientSeDto.setGeslacht(persoon.getGeslacht().getMnem());
		clientSeDto.setTelefoonnummer1(persoon.getTelefoonnummer1());
		clientSeDto.setTelefoonnummer2(persoon.getTelefoonnummer2());
		clientSeDto.setEmailadres(persoon.getEmailadres());
		clientSeDto.setDoelgroep(dossier.getDoelgroep());
		clientSeDto.setDubbeleTijdReden(dossier.getDubbeleTijdReden());
		clientSeDto.setInTehuis(client.getMammaDossier().getTehuis() != null);
		return clientSeDto;
	}

	private AdresSeDto createAdresSeDto(Adres gbaAdres)
	{
		if (gbaAdres == null)
		{
			return null;
		}
		AdresSeDto adres = new AdresSeDto();
		adres.setId(gbaAdres.getId());
		adres.setLocatieBeschrijving(AdresUtil.getAdres(gbaAdres));
		adres.setPostcode(gbaAdres.getPostcode());
		adres.setPlaats(gbaAdres.getPlaats());

		return adres;
	}

	private TijdelijkAdresSeDto createTijdelijkAdresSeDto(TijdelijkAdres tijdelijkAdres)
	{
		if (tijdelijkAdres == null)
		{
			return null;
		}

		TijdelijkAdresSeDto adres = new TijdelijkAdresSeDto();
		adres.setId(tijdelijkAdres.getId());
		adres.setStraat(tijdelijkAdres.getStraat());
		adres.setHuisnummer(tijdelijkAdres.getHuisnummer());
		adres.setHuisletter(tijdelijkAdres.getHuisletter());
		adres.setHuisnummerToevoeging(tijdelijkAdres.getHuisnummerToevoeging());
		adres.setHuisnummerAanduiding(tijdelijkAdres.getHuisnummerAanduiding());
		adres.setPostcode(tijdelijkAdres.getPostcode());
		adres.setPlaats(tijdelijkAdres.getPlaats());
		adres.setStartDatum(tijdelijkAdres.getStartDatum());
		adres.setEindDatum(tijdelijkAdres.getEindDatum());

		return adres;
	}

	private AfspraakStatusSe toAfspraakStatusSe(MammaAfspraakStatus mammaAfspraakStatus)
	{
		switch (mammaAfspraakStatus)
		{
		case GEPLAND:
			return AfspraakStatusSe.VERWACHT;
		case INGESCHREVEN:
			return AfspraakStatusSe.INGESCHREVEN;
		case ONDERZOEK:
			return AfspraakStatusSe.ONDERZOEK;
		case SIGNALEREN:
			return AfspraakStatusSe.SIGNALEREN;
		case BEEINDIGD:
			return AfspraakStatusSe.BEEINDIGD;
		default:
			throw new IllegalArgumentException("Afspraak met MammaAfspraakStatus " + mammaAfspraakStatus + " mag niet in de SE komen.");
		}
	}

	private OnderzoekSeDto createHuidigOnderzoekDto(MammaAfspraak afspraak)
	{
		return new OnderzoekDtoMapper().createOnderzoekDto(afspraak.getOnderzoek());
	}

	private MammografieSeDto createMammografieDto(MammaAfspraak afspraak)
	{
		return new MammografieDtoMapper().createMammografieDto(afspraak);
	}

	private SignalerenSeDto createSignalerenDto(MammaAfspraak afspraak)
	{
		return new SignalerenDtoMapper().createSignalerenDto(afspraak.getOnderzoek());
	}

}
