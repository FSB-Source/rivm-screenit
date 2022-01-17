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

import javax.validation.Valid;

import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordVergetenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.WachtwoordVergetenValidator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/wachtwoord")
public class WachtwoordVergetenController extends BaseController
{

	private static final Logger LOG = LoggerFactory.getLogger(WachtwoordVergetenController.class);

	@Autowired
	private WachtwoordVergetenValidator wachtwoordVergetenValidator;

	@Autowired
	private HuisartsService huisartsService;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{
		binder.addValidators(wachtwoordVergetenValidator);
	}

	@RequestMapping(method = RequestMethod.POST, path = "/vergeten")
	public ResponseEntity postWachtwoordVergeten(@Valid @RequestBody WachtwoordVergetenDto wachtwoordDto, BindingResult result, Model model)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(result.getAllErrors());
		}

		try
		{
			Huisarts huisarts = huisartsService.getHuisartsWith(wachtwoordDto);
			huisarts = huisartsService.wachtwoordVergeten(huisarts);
			synchronisatieService.syncHuisarts(huisarts);
		}
		catch (IllegalStateException e)
		{
			LOG.error(e.getMessage(), e);
			result.addError(new ObjectError("error.wrong.status", e.getMessage()));
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(result.getAllErrors());
		}

		return ResponseEntity.ok(null);
	}
}
