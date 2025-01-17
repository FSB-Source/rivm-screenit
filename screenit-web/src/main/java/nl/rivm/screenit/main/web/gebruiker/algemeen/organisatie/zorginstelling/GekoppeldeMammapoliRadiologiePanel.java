package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.zorginstelling;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Mammapoli;
import nl.rivm.screenit.model.RadiologieAfdeling;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GekoppeldeMammapoliRadiologiePanel extends GenericPanel<ZorgInstelling>
{
	@SpringBean
	private InstellingService instellingService;

	public GekoppeldeMammapoliRadiologiePanel(String id, IModel<ZorgInstelling> model)
	{
		super(id, model);

		var radiologieAfdelingen = instellingService.getChildrenOrganisaties(getModelObject(), RadiologieAfdeling.class);
		var mammapolis = instellingService.getChildrenOrganisaties(getModelObject(), Mammapoli.class);
		add(new Label("mammapoliLijst", stringLocaties(mammapolis)));
		add(new Label("radiologieAfdelingLijst", stringLocaties(radiologieAfdelingen)));
	}

	protected String stringLocaties(List<? extends Instelling> instellingLijst)
	{
		var instellingNamen = instellingLijst.stream().map(Instelling::getNaam).collect(Collectors.toList());
		return !instellingNamen.isEmpty() ? String.join(", ", instellingNamen) : "Er zijn geen afdelingen gekoppeld";
	}
}
