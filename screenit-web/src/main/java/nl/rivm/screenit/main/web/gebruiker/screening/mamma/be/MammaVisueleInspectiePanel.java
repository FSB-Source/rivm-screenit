package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.InputStream;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SvgImage;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.mamma.MammaBaseAfbeeldingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.resource.IResource.Attributes;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaVisueleInspectiePanel extends AbstractBEAccordionPanel<MammaOnderzoek>
{
	@SpringBean
	private MammaBaseAfbeeldingService afbeeldingService;

	public MammaVisueleInspectiePanel(String id, IModel<MammaOnderzoek> model)
	{
		this(id, model, 4);
	}

	public MammaVisueleInspectiePanel(String id, IModel<MammaOnderzoek> model, int panelSize)
	{
		super(id, new CompoundPropertyModel<>(model), Model.of("Visuele inspectie"), panelSize);
		super.setIngeklapt(false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		boolean isAutoniem = !MammobridgeRole.anoniemeRollen().contains(ScreenitSession.get().getMammaHuidigeIDS7Role());
		panelContainer.add(new Label("afspraak.ingeschrevenDoor.medewerker.naamVolledig").setVisible(isAutoniem));

		panelContainer.add(new SvgImage("svgImage")
		{

			@Override
			protected InputStream getSvgImageData(Attributes attributes)
			{
				MammaOnderzoek onderzoek = MammaVisueleInspectiePanel.this.getModelObject();
				return afbeeldingService.createVisueleInspectieAfbeelding(onderzoek.getMammografie().getVisueleInspectieAfbeelding(),
					onderzoek.getAmputatie());
			}

		});

		panelContainer.add(new Label("mammografie.afgerondDoor.medewerker.naamVolledig").setVisible(isAutoniem));

	}

	public void setPanelSize(AjaxRequestTarget target, int size)
	{
		setPanelSize(size);
		refreshPanel(target);
	}
}
