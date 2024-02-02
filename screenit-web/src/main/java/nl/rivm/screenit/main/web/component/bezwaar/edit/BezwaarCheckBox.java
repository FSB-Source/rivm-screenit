package nl.rivm.screenit.main.web.component.bezwaar.edit;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientTooltipPanel;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.BezwaarType;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class BezwaarCheckBox extends GenericPanel<BezwaarViewWrapper>
{

	private static final long serialVersionUID = 1L;

	public BezwaarCheckBox(String id, IModel<BezwaarViewWrapper> model)
	{
		super(id, new CompoundPropertyModel<>(model));
		BezwaarViewWrapper wrapper = model.getObject();

		if (BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS.equals(wrapper.getType()) && !Boolean.TRUE.equals(wrapper.getActief()))
		{
			setVisible(false);
		}

		add(new CheckBox("actief"));
		add(new EnumLabel<BezwaarType>("type"));
		add(new Label("type.info", new IModel()
		{

			@Override
			public String getObject()
			{
				return getString(EnumStringUtil.getPropertyString(wrapper.getType()) + ".info");
			}
		})

			.setEscapeModelStrings(false));
		add(new ClientTooltipPanel("tooltip", getClientToolTipTypeByBezwaar(wrapper.getType()), false));
	}

	private ClientTooltipType getClientToolTipTypeByBezwaar(BezwaarType type)
	{
		switch (type)
		{
		case GEEN_WETENSCHAPPELIJK_ONDERZOEK:
			return ClientTooltipType.BEZWAAR_GEENWETENSCHAPPELIJKONDERZOEK;
		case GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK:
			return ClientTooltipType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK;
		case GEEN_SIGNALERING_VERWIJSADVIES:
			return ClientTooltipType.GEEN_SIGNALERING_VERWIJSADVIES;
		case GEEN_UITWISSELING_MET_DE_HUISARTS:
			return ClientTooltipType.BEZWAAR_GEENUITWISSELINGHUISARTS;
		case GEEN_KWALITEITSWAARBORGING:
			return ClientTooltipType.BEZWAAR_GEENKWALITEITSBORGING;
		case GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS:
			return ClientTooltipType.BEZWAAR_GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS;
		default:
			throw new IllegalStateException("Er is een nieuw bezwaar waar geen tooltip voor is aangemaakt");
		}
	}
}
