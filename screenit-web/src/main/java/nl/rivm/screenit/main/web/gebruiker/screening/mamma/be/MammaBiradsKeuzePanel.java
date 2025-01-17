package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaBiradsKeuzePanel extends GenericPanel<MammaLezing>
{

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/radio/biradsKeuze.js"));
	}

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	public MammaBiradsKeuzePanel(String id, IModel<MammaLezing> model, MammaLezingParameters lezingParameters)
	{
		super(id, model);
		boolean amputatieRechts = (lezingParameters.getAmputatie() != null && MammaAmputatie.RECHTERBORST.equals(lezingParameters.getAmputatie()));
		addBiradsKeuzes("biradsRechts", lezingParameters.isInzien(), lezingParameters.isVerwijzenRechtsVerplicht(), amputatieRechts);
		boolean amputatieLinks = (lezingParameters.getAmputatie() != null && MammaAmputatie.LINKERBORST.equals(lezingParameters.getAmputatie()));
		addBiradsKeuzes("biradsLinks", lezingParameters.isInzien(), lezingParameters.isVerwijzenLinksVerplicht(), amputatieLinks);
	}

	private void addBiradsKeuzes(String wicketId, boolean alleenInzien, boolean verwijzenVerplicht, boolean isAmputatie)
	{
		List<MammaBIRADSWaarde> biradsWaardes = Arrays.asList(MammaBIRADSWaarde.EEN, MammaBIRADSWaarde.TWEE, MammaBIRADSWaarde.NUL, MammaBIRADSWaarde.VIER, MammaBIRADSWaarde.VIJF);

		RadioChoice<MammaBIRADSWaarde> biradsChoices = new RadioChoice<MammaBIRADSWaarde>(wicketId, biradsWaardes, new NaamChoiceRenderer<>())
		{
			@Override
			protected String getPrefix(int index, MammaBIRADSWaarde choice)
			{
				List<String> classes = new ArrayList<>();
				classes.add("radio");
				if (baseBeoordelingService.isBiradsVerwijzen(choice))
				{
					classes.add("biradsverwijzen");
				}
				else if (verwijzenVerplicht && !alleenInzien)
				{
					classes.add("onmogelijk");
				}
				return "<label class=\"" + StringUtils.join(classes, ' ') + "\">";
			}

			@Override
			public String getSuffix()
			{
				return "<span class=\"checkmark\"></span></label>";
			}
		};

		biradsChoices.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				onBiradsKeuzeChange(ajaxRequestTarget);
			}
		});

		biradsChoices.setEnabled(!alleenInzien && !isAmputatie);
		add(biradsChoices);
	}

	public void onBiradsKeuzeChange(AjaxRequestTarget target)
	{
	}
}
