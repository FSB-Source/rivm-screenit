package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.digitaleberichten;

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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.model.DigitaalBerichtTemplate;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.service.DigitaalBerichtTemplateService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class DigitaalBerichtTemplatesPanel extends Panel
{
	private ScreenitDropdown<DigitaalBerichtTemplateType> berichtSelectie;

	@SpringBean
	private DigitaalBerichtTemplateService templateService;

	protected DigitaalBerichtTemplatesPanel(String id, DigitaalBerichtType digitaalBerichtType)
	{
		super(id);
		IModel<DigitaalBerichtTemplateType> digitaalBerichtFilter = new Model<>();
		var selecteerEenTemplate = maakSelecteerEenTemplateLabel();
		maakFormMetDropdown(digitaalBerichtFilter, digitaalBerichtType, selecteerEenTemplate);
		maakBvoFilter(digitaalBerichtType, selecteerEenTemplate);

		add(new EmptyPanel("digitaalBerichtTemplateAanpassen").setOutputMarkupPlaceholderTag(true));
		setOutputMarkupId(true);
	}

	private Label maakSelecteerEenTemplateLabel()
	{
		var selecteerEenTemplate = new Label("selecteerEenTemplate", "Maak uw keuze in bericht type om een bericht aan te passen");
		selecteerEenTemplate.setOutputMarkupPlaceholderTag(true);
		add(selecteerEenTemplate);
		return selecteerEenTemplate;
	}

	private void maakBvoFilter(DigitaalBerichtType digitaalBerichtType, Label selecteerEenTemplate)
	{
		BvoZoekCriteria zoekCriteria = new BvoZoekCriteria();
		zoekCriteria.setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
		add(new FilterBvoFormPanel<>("bvoFilter", Model.of(zoekCriteria))
		{
			@Override
			protected void doFilter(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
			{
				var choices = getFilterOpties(digitaalBerichtType, filterModel.getObject().getBevolkingsonderzoeken());
				berichtSelectie.setChoices(choices);
				if (choices.isEmpty())
				{
					DigitaalBerichtTemplatesPanel.this.addOrReplace(new EmptyPanel("digitaalBerichtTemplateAanpassen"));
					selecteerEenTemplate.setVisible(true);
				}
				target.add(DigitaalBerichtTemplatesPanel.this);
			}
		});
	}

	private void maakFormMetDropdown(IModel<DigitaalBerichtTemplateType> digitaalBerichtFilter, DigitaalBerichtType digitaalBerichtType, Label selecteerEenTemplate)
	{
		var form = new Form<>("form", digitaalBerichtFilter);
		berichtSelectie = new ScreenitDropdown<>("digitaalBerichtFilter", digitaalBerichtFilter, getFilterOpties(digitaalBerichtType, ScreenitSession.get().getOnderzoeken()),
			new EnumChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(DigitaalBerichtTemplateType object)
				{
					return EnumStringUtil.maakStringMetBvoEnEnumPropertyString(object, DigitaalBerichtTemplatesPanel.this::getString);
				}
			});
		berichtSelectie.setNullValid(true);
		berichtSelectie.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				var type = digitaalBerichtFilter.getObject();
				if (type != null)
				{
					var template = templateService.findDigitaalBerichtTemplate(type).orElseGet(() -> maakNieuweDigitaalBerichtTemplate(type));
					var templateModel = ModelUtil.ccModel(template);
					addOrReplace(new DigitaalBerichtTemplateAanpassenPanel("digitaalBerichtTemplateAanpassen", templateModel));
					selecteerEenTemplate.setVisible(false);
				}
				else
				{
					addOrReplace(new EmptyPanel("digitaalBerichtTemplateAanpassen"));
					selecteerEenTemplate.setVisible(true);
				}
				target.add(DigitaalBerichtTemplatesPanel.this);
			}
		});
		form.add(berichtSelectie);
		form.setOutputMarkupId(true);
		add(form);
	}

	private DigitaalBerichtTemplate maakNieuweDigitaalBerichtTemplate(DigitaalBerichtTemplateType type)
	{
		var nieuweTemplate = new DigitaalBerichtTemplate();
		nieuweTemplate.setType(type);
		return nieuweTemplate;
	}

	private List<DigitaalBerichtTemplateType> getFilterOpties(DigitaalBerichtType digitaalBerichtType, List<Bevolkingsonderzoek> huidigeOnderzoeken)
	{
		var templateTypesMetJuisteBerichtType = Arrays.stream(DigitaalBerichtTemplateType.values()).filter(type -> type.getBerichtType().equals(digitaalBerichtType)).collect(
			Collectors.toList());
		return templateTypesMetJuisteBerichtType.stream().filter(bericht -> huidigeOnderzoeken.stream()
			.anyMatch(onderzoek -> bericht.getBevolkingsonderzoeken().contains(onderzoek))).collect(Collectors.toList());
	}
}
