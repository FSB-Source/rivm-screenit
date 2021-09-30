package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class IndexController
{
	@RequestMapping(value = "/registreren/", method = RequestMethod.GET)
	public String redirectRegistrerenWithSlash()
	{
		return "redirect:" + "../#/registreren";
	}

	@RequestMapping(value = "/registreren", method = RequestMethod.GET)
	public String redirectRegistreren()
	{
		return "redirect:" + "./#/registreren";
	}

	@RequestMapping(value = "/registreren/{code}", method = RequestMethod.GET)
	public String redirectRegistrerenMetParam(@PathVariable String code)
	{
		return "redirect:" + "../#/registreren/" + code;
	}

	@RequestMapping(value = "/wachtwoordvergeten/registreren", method = RequestMethod.GET)
	public String redirectRegistrerenWachtwoordVergeten()
	{
		return "redirect:" + "../#/wachtwoordvergeten/registreren";
	}
}
