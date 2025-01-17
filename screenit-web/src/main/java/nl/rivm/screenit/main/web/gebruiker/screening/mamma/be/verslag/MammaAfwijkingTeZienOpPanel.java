package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag;

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

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAfwijkingTeZienOp;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.validator.RangeValidator;

public class MammaAfwijkingTeZienOpPanel extends GenericPanel<MammaLezing>
{

	public MammaAfwijkingTeZienOpPanel(String id, IModel<MammaLezing> model)
	{
		super(id, model);
		add(maakAfwijkingTeZienOp());
	}

	private WebMarkupContainer maakAfwijkingTeZienOp()
	{
		var afwijkingTeZienOpContainer = new WebMarkupContainer("afwijkingTeZienOpContainer");
		var afwijkingTeZienOp = ComponentHelper.addRadioChoice(afwijkingTeZienOpContainer, "afwijkingTeZienOp", MammaAfwijkingTeZienOp.class);
		afwijkingTeZienOp.setRequired(true);

		var teZienSliceCC = maakSliceInputField("zichtbaarOpCcNummer");
		var teZienSliceMLO = maakSliceInputField("zichtbaarOpMloNummer");

		var sliceExtraInfoContainer = new WebMarkupContainer("sliceExtraInfoContainer");

		verwerkAfwijkingZichtbaarOpStatus(getModelObject(), sliceExtraInfoContainer, teZienSliceCC, teZienSliceMLO);
		sliceExtraInfoContainer.setOutputMarkupPlaceholderTag(true);
		afwijkingTeZienOp.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				var lezing = getModelObject();
				verwerkAfwijkingZichtbaarOpStatus(lezing, sliceExtraInfoContainer, teZienSliceCC, teZienSliceMLO);
				target.add(sliceExtraInfoContainer, teZienSliceCC, teZienSliceMLO);
			}

		});
		sliceExtraInfoContainer.add(teZienSliceCC, teZienSliceMLO);
		afwijkingTeZienOpContainer.add(afwijkingTeZienOp);
		afwijkingTeZienOpContainer.add(sliceExtraInfoContainer);
		return afwijkingTeZienOpContainer;
	}

	private TextField<Integer> maakSliceInputField(String id)
	{
		var teZienSlice = new TextField<>(id, Integer.class);
		teZienSlice.setOutputMarkupPlaceholderTag(true);
		teZienSlice.setRequired(true);
		teZienSlice.add(new RangeValidator<>(1, 99));
		return teZienSlice;
	}

	private void verwerkAfwijkingZichtbaarOpStatus(MammaLezing lezing, WebMarkupContainer sliceExtraInfoContainer,
		TextField<Integer> teZienSliceCC, TextField<Integer> teZienSliceMLO)
	{
		var afwijkingTeZienOp = lezing.getAfwijkingTeZienOp();
		if (afwijkingTeZienOp != null)
		{
			switch (afwijkingTeZienOp)
			{
			case ALLEEN_SLICE_MLO:
				sliceExtraInfoContainer.setVisible(true);
				updateSlices(false, true, lezing, teZienSliceCC, teZienSliceMLO);
				break;
			case ALLEEN_SLICE_CC:
				sliceExtraInfoContainer.setVisible(true);
				updateSlices(true, false, lezing, teZienSliceCC, teZienSliceMLO);
				break;
			case C_VIEW_EN_SLICES:
				sliceExtraInfoContainer.setVisible(false);
				updateSlices(false, false, lezing, teZienSliceCC, teZienSliceMLO);
				break;
			case SLICES_CC_EN_MLO:
				sliceExtraInfoContainer.setVisible(true);
				updateSlices(true, true, lezing, teZienSliceCC, teZienSliceMLO);
				break;
			}
		}
		else
		{
			sliceExtraInfoContainer.setVisible(false);
			lezing.setZichtbaarOpCcNummer(null);
			lezing.setZichtbaarOpMloNummer(null);
		}
	}

	private void updateSlices(boolean sliceCC, boolean sliceMLO, MammaLezing lezing, TextField<Integer> teZienSliceCC, TextField<Integer> teZienSliceMLO)
	{
		teZienSliceMLO.setVisible(sliceMLO);
		teZienSliceCC.setVisible(sliceCC);
		if (!sliceCC)
		{
			lezing.setZichtbaarOpCcNummer(null);
		}
		if (!sliceMLO)
		{
			lezing.setZichtbaarOpMloNummer(null);
		}
	}
}
