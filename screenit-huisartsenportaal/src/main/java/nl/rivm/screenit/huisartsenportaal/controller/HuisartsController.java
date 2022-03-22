package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import javax.validation.Valid;

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.MederwerkerDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Overeenkomst;
import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;
import nl.rivm.screenit.huisartsenportaal.repository.OvereenkomstRepository;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.GebruikersnaamValidator;
import nl.rivm.screenit.huisartsenportaal.validator.HuisartsValidator;
import nl.rivm.screenit.huisartsenportaal.validator.WachtwoordRegistrerenValidator;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/huisarts")
@PreAuthorize("isAuthenticated()")
public class HuisartsController extends BaseController
{

	@Autowired
	private GebruikersnaamValidator gebruikersnaamValidator;

	@Autowired
	private HuisartsValidator huisartsValidator;

	@Autowired
	private WachtwoordRegistrerenValidator wachtwoordRegistrerenValidator;

	@Autowired
	private HuisartsService huisartsService;

	@Autowired
	private SynchronisatieService syncService;

	@Autowired
	private OvereenkomstRepository overeenkomstRepository;

	@Autowired
	private ModelMapper modelMapper;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{
		binder.addValidators(gebruikersnaamValidator, wachtwoordRegistrerenValidator, huisartsValidator);
	}

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<HuisartsDto> getHuisarts()
	{
		Huisarts arts = getIngelogdeHuisarts();
		if (arts != null)
		{
			HuisartsDto dto = new HuisartsDto();
			modelMapper.map(getIngelogdeHuisarts(), dto);
			dto.setUsername(arts.getGebruikersnaam());
			return new ResponseEntity<>(dto, HttpStatus.OK);
		}
		else
		{
			return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
		}
	}

	@RequestMapping(value = "/controle", method = RequestMethod.PUT)
	public ResponseEntity putControleHuisarts(@Valid @RequestBody HuisartsDto huisartsDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}
		return ResponseEntity.ok().body(huisartsDto);
	}

	@RequestMapping(method = RequestMethod.PUT)
	public ResponseEntity putHuisarts(@Valid @RequestBody HuisartsDto huisartsDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}

		Huisarts huisarts = getIngelogdeHuisarts();
		huisarts = huisartsService.updateHuisarts(huisartsDto, huisarts);
		syncService.syncHuisarts(huisarts);

		HuisartsDto dto = new HuisartsDto();
		modelMapper.map(getIngelogdeHuisarts(), dto);
		dto.setUsername(getIngelogdeHuisarts().getGebruikersnaam());
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

	@RequestMapping(value = "/currentuser", method = RequestMethod.GET)
	public ResponseEntity<MederwerkerDto> getCurrentUser()
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		if (huisarts != null)
		{
			return new ResponseEntity<>(getMedewerkerDtoFrom(huisarts), HttpStatus.OK);
		}
		return new ResponseEntity<>(HttpStatus.NOT_FOUND);
	}

	private MederwerkerDto getMedewerkerDtoFrom(Huisarts huisarts)
	{
		MederwerkerDto dto = new MederwerkerDto();
		dto.setRollen(huisarts.getRollen());
		dto.setId(huisarts.getHuisartsportaalId());

		Overeenkomst overeenkomst = overeenkomstRepository.findFirstByOrderByLaatsteWijzigDatumDesc();
		if (overeenkomst != null && huisarts.getOvereenkomstGeaccordeerdDatum() != null)
		{
			Boolean getekend = overeenkomst.getLaatsteWijzigDatum().before(huisarts.getOvereenkomstGeaccordeerdDatum());
			dto.setOvereenkomstGetekend(getekend);
			if (!getekend)
			{
				List<Recht> overeenkomstRechten = new ArrayList<>();

				if (dto.getRollen().contains(Recht.ROLE_REGISTEREN))
				{
					overeenkomstRechten.add(Recht.ROLE_REGISTEREN);
				}

				overeenkomstRechten.add(Recht.ROLE_OVEREENKOMST);
				dto.setRollen(overeenkomstRechten);
			}
		}
		if (huisarts.getRollen().size() == 0 && huisarts.getOvereenkomstGeaccordeerdDatum() == null)
		{
			dto.setOvereenkomstGetekend(true);
		}
		return dto;
	}
}
