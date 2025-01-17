package nl.rivm.screenit.main.web.gebruiker.screening.cervix.huisarts;

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

import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class CervixHuisartsOpvraagPanel extends Panel
{
	@SpringBean
	private CervixHuisartsService huisartsService;

	private String agbCode;

	private final boolean onbekendeArtsNieuweAanmaken;

	public CervixHuisartsOpvraagPanel(String id)
	{
		this(id, true);
	}

	public CervixHuisartsOpvraagPanel(String id, boolean onbekendeArtsNieuwAanmaken)
	{
		super(id);
		this.onbekendeArtsNieuweAanmaken = onbekendeArtsNieuwAanmaken;

		Form<CervixHuisartsOpvraagPanel> form = new Form<>("form", new CompoundPropertyModel<>(this));
		add(form);

		FormComponent<Integer> agbCodeField = ComponentHelper.addTextField(form, "agbCode", true, 8, Integer.class, false);

		IndicatingAjaxSubmitLink huisartsZoekButton = new IndicatingAjaxSubmitLink("submit", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String agbCodeFormatted = StringUtils.leftPad(agbCode, 8, '0');
				if (agbCodeFormatted.length() > 8)
				{
					error(getString("agbCode.IConverter.Integer"));
					return;
				}
				CervixHuisarts arts;
				if (onbekendeArtsNieuweAanmaken)
				{
					arts = huisartsService.getUitstrijkendArtsMetAgb(agbCodeFormatted);
				}
				else
				{
					arts = huisartsService.getHuisartsMetAgbCode(agbCodeFormatted);
				}
				agbCode = null;
				target.add(agbCodeField);
				setUitstrijkendArts(target, arts);
			}
		};
		form.setDefaultButton(huisartsZoekButton);
		form.add(huisartsZoekButton);
	}

	protected abstract void setUitstrijkendArts(AjaxRequestTarget target, CervixHuisarts arts);

}
