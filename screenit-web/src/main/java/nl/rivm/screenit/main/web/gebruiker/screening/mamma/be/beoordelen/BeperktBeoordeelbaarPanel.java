package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;

import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class BeperktBeoordeelbaarPanel extends GenericPanel<MammaLezing>
{

	private final boolean alleenInzien;

	private ScreenitDropdown<MammaBeperktBeoordeelbaarReden> dropdown;

	private WebMarkupContainer beperktBeoordeelbaarTekstContainer;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	public BeperktBeoordeelbaarPanel(String id, IModel<MammaLezing> model, boolean alleenInzien)
	{
		super(id, model);
		this.alleenInzien = alleenInzien;
		setOutputMarkupId(true);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		beperktBeoordeelbaarTekstContainer = new WebMarkupContainer("beperktBeoordeelbaarTekstContainer");
		add(beperktBeoordeelbaarTekstContainer);
		List<MammaBeperktBeoordeelbaarReden> beperktBeoordeelbaarRedenen = Arrays.asList(MammaBeperktBeoordeelbaarReden.values());
		dropdown = new ScreenitDropdown<>("beperktBeoordeelbaarReden", beperktBeoordeelbaarRedenen, new EnumChoiceRenderer<>());
		dropdown.setOutputMarkupId(true);
		dropdown.setVisible(!alleenInzien);
		add(dropdown);

		MammaBeperktBeoordeelbaarReden beperktBeoordeelbaarReden = getModelObject().getBeperktBeoordeelbaarReden();
		EnumLabel<MammaBeperktBeoordeelbaarReden> beperktBeoordeelbaarRedenText = new EnumLabel<>("beperktBeoordeelbaarRedenText", beperktBeoordeelbaarReden);
		beperktBeoordeelbaarRedenText.setVisible(alleenInzien);
		add(beperktBeoordeelbaarRedenText);

		beperktBeoordeelbaarTekstContainer.setOutputMarkupPlaceholderTag(true);
		beperktBeoordeelbaarTekstContainer.setOutputMarkupId(true);
		beperktBeoordeelbaarTekstContainer.setVisible(MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK == beperktBeoordeelbaarReden);
		final TextArea<String> waaromGeenBeoordelingMogelijkTextArea = new TextArea<>("waaromGeenBeoordelingMogelijk");
		waaromGeenBeoordelingMogelijkTextArea.setEnabled(!alleenInzien);
		MammaBeoordelenLezingPanel.tuneTextArea(beperktBeoordeelbaarTekstContainer, waaromGeenBeoordelingMogelijkTextArea, alleenInzien, true);

		setVisible(!alleenInzien || beperktBeoordeelbaarReden != null);

		dropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				updateGeenBeoordelingMogelijk(target);
			}
		});

		updateDropdownClickable();
	}

	public void updateGeenBeoordelingMogelijk(AjaxRequestTarget target)
	{
		boolean geenBeoordelingMogelijk = MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK == BeperktBeoordeelbaarPanel.this.getModelObject()
			.getBeperktBeoordeelbaarReden();
		beperktBeoordeelbaarTekstContainer.setVisible(geenBeoordelingMogelijk);
		target.add(beperktBeoordeelbaarTekstContainer);
		if (!geenBeoordelingMogelijk)
		{
			getModelObject().setWaaromGeenBeoordelingMogelijk(null);
		}
	}

	public void updateDropdown(AjaxRequestTarget target)
	{
		updateDropdownClickable();
		updateGeenBeoordelingMogelijk(target);
		target.add(this);
		target.add(dropdown);
	}

	public void updateDropdownClickable()
	{
		MammaLezing lezing = getModelObject();

		if (baseBeoordelingService.isLezingVerwijzen(lezing))
		{

			getModelObject().setBeperktBeoordeelbaarReden(null);
			dropdown.setEnabled(false);
		}
		else
		{
			dropdown.setEnabled(true);
		}
	}
}
