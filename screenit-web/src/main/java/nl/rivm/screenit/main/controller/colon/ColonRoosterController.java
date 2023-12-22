package nl.rivm.screenit.main.controller.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakSlotService;
import nl.rivm.screenit.main.service.colon.ColonBlokkadeService;
import nl.rivm.screenit.main.service.colon.ColonRoosterBeperkingService;
import nl.rivm.screenit.main.service.colon.LocatieService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.colon.ColonAfspraakSlotMapper;
import nl.rivm.screenit.mappers.colon.ColonBlokkadeMapper;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakSlotDto;
import nl.rivm.screenit.model.colon.dto.ColonBlokkadeDto;
import nl.rivm.screenit.model.colon.dto.ColonRoosterBeperkingenDto;
import nl.rivm.screenit.model.colon.dto.ColonRoosterInstellingenDto;
import nl.rivm.screenit.model.colon.dto.KamerDto;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/colon/rooster")
public class ColonRoosterController
{
	private final RoosterService roosterService;

	private final HibernateService hibernateService;

	private final LocatieService locatieService;

	private final ColonUitnodigingService uitnodigingService;

	private final ColonBlokkadeMapper blokkadeMapper;

	private final ColonRoosterBeperkingService beperkingService;

	private final ColonAfspraakSlotService afspraakSlotService;

	private final ColonBlokkadeService blokkadeService;

	private final ColonAfspraakSlotMapper afspraakSlotMapper;

	@GetMapping("/afspraaksloten")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<ColonAfspraakSlotDto> getAfspraakSloten(@RequestParam("startDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
		@RequestParam("endDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate)
	{
		var intakeLocatie = ScreenitSession.get().getColoscopieCentrum();

		if (intakeLocatie == null)
		{
			throw new IllegalStateException("error.geen.intakelocatie");
		}

		if (startDate == null)
		{
			throw new IllegalStateException("error.geen.start.datum");
		}

		if (endDate == null)
		{
			throw new IllegalStateException("error.geen.eind.datum");
		}

		var filter = new RoosterListViewFilter();

		filter.setStartDatum(DateUtil.toUtilDate(startDate));
		filter.setEndDatum(DateUtil.toUtilDate(endDate));

		var items = roosterService.getAlleRoosterBlokkenInPeriode("startTime", true, filter, intakeLocatie);
		var afspraakSloten = new ArrayList<ColonAfspraakSlotDto>();
		for (var item : items)
		{
			var roosterItem = hibernateService.load(RoosterItem.class, item.getRoosterItemId());
			var roosterItemStatus = roosterService.getRoosterItemStatus(roosterItem);
			item.setStatus(roosterItemStatus);
			afspraakSloten.add(afspraakSlotMapper.roosterListItemViewWrapperToColonAfspraakDto(item));
		}
		return afspraakSloten;
	}

	@PostMapping("/afspraaksloten")
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> createAfspraakSloten(@RequestBody ColonAfspraakSlotDto afspraakSlotenDto) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		afspraakSlotService.createAfspraakSlot(afspraakSlotenDto, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@PutMapping("/afspraaksloten/{id}")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> updateAfspraakSlot(@PathVariable("id") Long id, @RequestBody ColonAfspraakSlotDto afspraakSlotDto)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		afspraakSlotService.updateAfspraakSlot(id, afspraakSlotDto, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@DeleteMapping("/afspraaksloten/{id}")
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> deleteAfspraakSlot(@PathVariable("id") Long id)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		afspraakSlotService.deleteAfspraakSlot(id, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@GetMapping("/blokkades")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<ColonBlokkadeDto> getRoosterBlokkades(@RequestParam("startDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
		@RequestParam("endDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate)
	{
		if (startDate == null)
		{
			throw new IllegalStateException("error.geen.start.datum");
		}

		if (endDate == null)
		{
			throw new IllegalStateException("error.geen.eind.datum");
		}

		var start = DateUtil.toUtilDate(startDate);
		var end = DateUtil.toUtilDate(endDate);

		var periode = new Periode(start, end);
		return roosterService.getBlokkades(periode, null).stream().map(blokkadeMapper::colonBlokkadeToDto).collect(Collectors.toList());
	}

	@PostMapping("/blokkades")
	@SecurityConstraint(actie = Actie.TOEVOEGEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> createBlokkade(@RequestBody ColonBlokkadeDto blokkadeDto) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		blokkadeService.createBlokkade(blokkadeDto, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@DeleteMapping("/blokkades/{id}")
	@SecurityConstraint(actie = Actie.VERWIJDEREN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> deleteBlokkade(@PathVariable("id") Long id) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		blokkadeService.deleteBlokkade(id, ScreenitSession.get().getLoggedInInstellingGebruiker());
		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@GetMapping("/kamers")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<KamerDto> getKamers()
	{
		var intakeLocatie = ScreenitSession.get().getColoscopieCentrum();

		if (intakeLocatie == null)
		{
			throw new IllegalStateException("error.geen.intakelocatie");
		}

		return locatieService.getKamers(intakeLocatie).stream().map(KamerDto::fromKamer).collect(Collectors.toList());
	}

	@GetMapping("/beperkingen")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonRoosterBeperkingenDto> getRoosterBeperkingen()
	{
		var roosterBeperkingen = beperkingService.getRoosterBeperkingen();
		return ResponseEntity.ok(roosterBeperkingen);
	}

	@PutMapping("/beperkingen")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> updateRoosterBeperkingen(@RequestBody ColonRoosterBeperkingenDto roosterBeperkingenDto) throws ValidatieException
	{
		beperkingService.updateRoosterBeperkingen(roosterBeperkingenDto);
		return ResponseEntity.ok().build();
	}

	@GetMapping("/instellingen")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_LOCATIE_NIEUW_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ColonRoosterInstellingenDto getInstellingen()
	{
		var instellingen = new ColonRoosterInstellingenDto();
		instellingen.setGeprognotiseerdeVanafDatum(uitnodigingService.getGeprognotiseerdeIntakeDatum(true).with(DayOfWeek.MONDAY));
		instellingen.setDuurAfspraakInMinuten(ScreenitSession.get().getColoscopieCentrum().getAfspraakDefinities().get(0).getDuurAfspraakInMinuten());

		return instellingen;
	}
}
