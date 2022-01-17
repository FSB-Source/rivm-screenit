import FooterView, {FooterViewProps} from "./FooterView"
import {connect} from "react-redux"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): FooterViewProps => {
	return {
		environmentInfo: state.environmentInfo,
	}
}

const FooterContainer = connect(mapStateToProps)(FooterView)
export default FooterContainer